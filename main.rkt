#lang racket

(require (for-syntax syntax/parse syntax/parse/class/struct-id))
(require "./private/lens.rkt"
         "./private/traversal.rkt"
         "./private/isomorphism.rkt"
         "./private/optic.rkt")
(module+ test (require rackunit))

(provide (all-defined-out))

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



; the scrutinee of the current update form.
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
       #:datum-literals (cons list list-of struct-field _)
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
       [_ #'body]
       [var:id #'(let ([var optic-so-far]) body)])]))

(define-for-syntax (get-struct-pred-id struct-id-stx)
  (syntax-parse struct-id-stx
    [s:struct-id
     (attribute s.predicate-id)]))

(module+ test
  (check-equal? (update (list 1 2) [(cons a (cons b _)) (set a #t) (set b #f)])
                (list #t #f))
  (check-equal? (update (list 1 2) [(list a b) (set a #t) (set b #f)])
                (list #t #f))
  (check-equal? (update (list 7 8 9)
                        [(list a b c)
                         (set a 1)
                         (set b (get a))
                         (define x (get c))
                         (set c (add1 x))])
                (list 1 1 10))
  (struct posn [x y] #:transparent)
  (check-equal? (update (posn 1 2) [(struct-field posn x a) (set a 3)]) (posn 3 2))
  (check-equal? (update (posn 1 2) [(struct-field posn x) (set x 3)]) (posn 3 2))
  (check-equal? (update (posn (cons 1 2) 3) [(struct-field posn x (cons a b)) (modify a -) (modify b sqr)]) (posn (cons -1 4) 3))
  (check-equal? (update (list 1 2) [(list a b) (modify a -) (set b #t)]) (list -1 #t))
  (check-equal? (update (list 1 2 3 4) [(list-of a) (modify a -)]) '(-1 -2 -3 -4))
  (check-equal? (update '((1 2) (3 4) (5 6)) [(list-of (list a _)) (modify a -)]) '((-1 2) (-3 4) (-5 6)))
  (check-equal? (update '(((1 2) (3)) ((4) ())) [(list-of (list-of (list-of a))) (modify a -)]) '(((-1 -2) (-3)) ((-4) ())))
  (check-equal? (update '(1 2) [(list a b) (get a)]) 1)
  (check-pred lens? (update '(1 2) [(list a b) a]))
  ; update in an update works
  (check-equal? (update '(1 (2 3)) [(list a b)
                                    (set b (update (get b) [(list c d) (set d 4)]))
                                    (set a #t)])
                '(#t (2 4))))
