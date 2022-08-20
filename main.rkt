#lang racket

(require (for-syntax syntax/parse syntax/parse/class/struct-id))
(require "./private/lens.rkt"
         "./private/traversal.rkt"
         "./private/isomorphism.rkt"
         "./private/optic.rkt"
         "./private/prism.rkt")
(module+ test (require rackunit))

(provide
 #;(update expr [pattern body ...+])
 ; like match, but can also be used for immutably updating values.
 ; Use get, set, modify, etc. on pattern-bound variables.
 ; Under the hood, variables are bound to optics like lenses and traversals
 update
 #;(-> lens? any/c)
 ; get the value
 get
 #;(-> lens? any/c any/c)
 ; set the value
 set
 #;(-> lens? (-> any/c any/c) any/c)
 ; apply a function to update the value(s)
 modify
 #;(-> lens? (-> A B B) B B)
 ; foldl over the values
 fold)

#|
; happy birthday dad!
(update parents
  [(list (person dad-name dad-age) (person mom-name mom-age))
   (set dad-age (add1 (get dad-age))])
; returns a copy of the same list, but dad's age is incremented

going through the pattern binds optics to variable names
executing the body should run the specified updates.
The body can have many forms, and get, set, modify, etc. are procedures.
"mutator" procedures like 'set' actually mutate a reference to a copy of the target variable and return the new
value. The user has first-class access to the optics, their foci, and can use them to procedurally update the target.
|#



; --- core compiler ---



; the scrutinee of the current update form. Does not change as the update dives into the structure.
(define current-update-target (make-parameter #f #f 'current-update-target))
; retrieve the focus of the current target under 'optic'
(define (get optic) (optic-get optic (current-update-target)))
; set the focus of the current target under 'optic'
(define (set optic focus)
  (current-update-target (optic-set optic (current-update-target) focus))
  (current-update-target))
; apply a function to update the focus of the current target under 'optic'
(define (modify optic func)
  (current-update-target (traversal-modify optic (current-update-target) func))
  (current-update-target))
; fold over the current target's foci under 'traversal'
(define (fold traversal proc init) (traversal-foldl traversal (current-update-target) proc init))

(define-syntax update
  (syntax-parser
    [(_ target-expr [pat body ...])
     #'(let ([target-var target-expr])
         ; for multi-clause, make sure you re-parameterize each clause
         (parameterize ([current-update-target target-var])
           (update* target-var pat identity-iso (begin body ...))))]))

; compile the patterns to nested bindings of variables to optics
(define-syntax update*
  (syntax-parser
    [(_ current-val:id pat optic-so-far:id body)
     (syntax-parse #'pat
       #:datum-literals (cons list list-of struct-field iso optic and2 ? _)
       [(cons car-pat cdr-pat)
        #'(if (cons? current-val)
              (let ([car-val (car current-val)]
                    [cdr-val (cdr current-val)]
                    [lens-with-car (optic-compose optic-so-far car-lens)]
                    [lens-with-cdr (optic-compose optic-so-far cdr-lens)])
                (update* car-val car-pat lens-with-car
                         (update* cdr-val cdr-pat lens-with-cdr
                                  body)))
              (error 'update "expected a cons, got ~a" current-val))]
       [(list) #'(update* current-val _ optic-so-far body)]
       [(list pat0 pat ...)
        #'(update* current-val (cons pat0 (list pat ...)) optic-so-far body)]
       [(list-of pat)
        #'(if (list? current-val)
              (let ([new-optic (traversal-compose optic-so-far list-traversal)])
                (update* current-val pat new-optic body))
              (error 'update "expected a cons, got ~a" current-val))]
       [(struct-field struct-name:struct-id field-name:id (~optional field-pat #:defaults ([field-pat #'field-name])))
        (define/syntax-parse struct-name? (get-struct-pred-id #'struct-name))
        #'(if (struct-name? current-val)
              (let* ([field-lens (struct-lens struct-name field-name)]
                     [field-value (lens-get field-lens current-val)]
                     [new-optic (optic-compose optic-so-far field-lens)])
                (update* field-value field-pat new-optic body))
              (error 'update "expected a ~a, got ~a" 'struct-name? current-val))]
       [(iso forward backward pat)
        #'(let* ([iso (make-iso forward backward)]
                 [new-optic (optic-compose optic-so-far iso)])
            (update* current-val pat new-optic body))]
       [(optic o pat)
        #'(let ([new-optic (optic-compose optic-so-far o)])
            (update* current-val pat new-optic body))]
       [(and2 pat1 pat2)
        #'(update* current-val pat1 optic-so-far (update* current-val pat2 optic-so-far body))]
       [(? predicate (~optional (~seq #:description desc)))
        #'(if (predicate current-val)
              body
              (error 'update (~? (~@ "expected a ~a, got ~a" desc current-val) "predicate check failed")))]
       [_ #'body]
       [var:id #'(let ([var optic-so-far]) body)])]))

(define-for-syntax (get-struct-pred-id struct-id-stx)
  (syntax-parse struct-id-stx
    [s:struct-id
     (attribute s.predicate-id)]))

(module+ test
  ; you can set values
  (check-equal? (update (list 1 2) [(list a b) (set a #t) (set b #f)])
                (list #t #f))
  ; you can apply functions to modify values
  (check-equal? (update (list 1 2) [(list a b) (modify b add1)])
                '(1 3))
  ; you can nest patterns to perform deep updates
  (check-equal? (update '(1 (2 3)) [(list a (list b c)) (set c #t)])
                '(1 (2 #t)))
  ; get, set, etc. are procedures and can be used in any expression position inside of an update form
  (check-equal? (update (list 7 8 9)
                        [(list a b c)
                         (set a 1)
                         (set b (get a))
                         (define x (get c))
                         (set c (add1 x))])
                (list 1 1 10))
  ; struct pattern
  (struct posn [x y] #:transparent)
  (check-equal? (update (posn 1 2) [(struct-field posn x a) (set a 3)]) (posn 3 2))
  ; without a pattern after the field name, the field's optic is bound to the name of the field
  (check-equal? (update (posn 1 2) [(struct-field posn x) (set x 3)]) (posn 3 2))
  ; list-of pattern creates a traversal which can modify all elements
  (check-equal? (update (list 1 2 3 4) [(list-of a) (modify a -)]) '(-1 -2 -3 -4))
  ; you can fold(l) the elements of a traversal
  (check-equal? (update (list 1 2 3) [(list-of a) (fold a cons '())])
                (list 3 2 1))
  ; if the last expression of the body is a get, its value is returned
  (check-equal? (update '(1 2) [(list a b) (get a)]) 1)
  ; you can access the optic directly
  (check-pred lens? (update '(1 2) [(list a b) a]))
  (test-equal? "can use iso to treat an X as a Y"
               (update 'foo [(iso symbol->string string->symbol str) (modify str string-upcase)])
               'FOO)
  (test-equal? "can specify an optic directly"
               (update (list 1 2) [(optic list-traversal a) (modify a -)])
               (list -1 -2))
  (test-equal? "and pattern"
               (update (list 1 2) [(and2 (list-of n) (list a b))
                                   (modify n -)
                                   (modify a number->string)])
               (list "-1" -2))
  (test-equal? "? pattern"
               (update (list 1 2) [(list (and2 (? odd?) a) (? even?)) (modify a -)])
               (list -1 2)))



(module+ test
  (check-equal? (update (list 1 2) [(cons a (cons b _)) (set a #t) (set b #f)])
                (list #t #f))
  (check-equal? (update (list 1 2) [(list a b) (modify a -) (set b #t)]) (list -1 #t))
  (check-equal? (update (posn (cons 1 2) 3) [(struct-field posn x (cons a b)) (modify a -) (modify b sqr)]) (posn (cons -1 4) 3))
  (check-equal? (update '((1 2) (3 4) (5 6)) [(list-of (list a _)) (modify a -)]) '((-1 2) (-3 4) (-5 6)))
  (check-equal? (update '(((1 2) (3)) ((4) ())) [(list-of (list-of (list-of a))) (modify a -)]) '(((-1 -2) (-3)) ((-4) ())))
  (test-equal? "update in an update works"
               (update '(1 (2 3)) [(list a b)
                                    (set b (update (get b) [(list c d) (set d 4)]))
                                    (set a #t)])
                '(#t (2 4)))
  (test-equal? "iso composes"
               (update '(foo bar) [(cons (iso symbol->string string->symbol str) _) (modify str string-upcase)])
               '(FOO bar))
  ; regression test:
  ; currently, the expanded code inserts structure checks and composes and binds optics.
  ; this structure check is/was broken for list-of.
  (test-equal? "list-of structs works"
               (update (list (posn 1 2) (posn 3 4)) [(list-of (struct-field posn x)) (modify x -)])
               (list (posn -1 2) (posn -3 4))))
