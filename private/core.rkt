#lang racket

(require (for-syntax syntax/parse syntax/parse/class/struct-id))
(require "../optics/lens.rkt"
         "../optics/traversal.rkt"
         "../optics/isomorphism.rkt"
         "../optics/optic.rkt"
         "../optics/prism.rkt")
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
 fold
 ; a parameter for the current update target. Gets updated by set, modify, etc.
 ; Useful for using optics with low-level functions. Ex:
 #;(update (list 1 2) [(list a b) (optic-set a (current-update-target) 3)])
 #;'(3 2)
 ; get, set, modify, etc. use this under the hood.
 ; NOTE: When using this parameter, be careful when performing multiple updates. You must update the parameter
 ; with its new value.
 current-update-target
 (for-syntax pattern-literals))

#|
; happy birthday dad!
(update parents
  [(list (person dad-name dad-age) (person mom-name mom-age))
   (set dad-age (add1 (get dad-age))])
; returns a copy of the same list, but dad's age is incremented

going through the pattern binds optics to variable names.
executing the body runs the specified updates.
a reference to the current target is set to the target's new value when you perform an update.
The user has first-class access to the optics, their foci, and can use them to procedurally update the target.
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

; like match, but supports immutable updates too
(define-syntax update
  (syntax-parser
    [(_ target-expr [pat0 body0 ...] clause ...)
     #'(let ([target target-expr])
         (update* target pat0 (begin body0 ...) (update target clause ...)))]
    [(_ target-expr)
     #'(error 'update "no matching clauses")]))

(define-syntax update*
  (syntax-parser
    [(_ target:id pat body on-fail)
     #'(let ([on-fail-proc (thunk on-fail)])
         (validate-target target pat
                          (bind-optics identity-iso pat
                                       (parameterize ([current-update-target target])
                                         body))
                          on-fail-proc))]))

; compile the patterns to nested bindings of variables to optics
(begin-for-syntax
  (define-literal-set pattern-literals
    #:datum-literals (cons list list-of struct-field iso optic and2 ? _)
    ()))

; check if the pattern matches the target successfully
; this could currently just evaluate to a boolean, but in case things get weird in the future, it is more general
(define-syntax validate-target
  (syntax-parser
    [(_ target:id pat body on-fail:id)
     (syntax-parse #'pat
       #:literal-sets (pattern-literals)
       [(cons car-pat cdr-pat)
        #'(if (cons? target)
            (let ([a (car target)]
                  [d (cdr target)])
              (validate-target a car-pat (validate-target d cdr-pat body on-fail) on-fail))
            (on-fail))]
       [(list) #'(if (null? target) body (on-fail))]
       [(list pat0 pat ...)
        #'(validate-target target (cons pat0 (list pat ...)) body on-fail)]
       [(list-of pat)
        #'(if (list? target)
              (validate-targets target pat body on-fail)
              (on-fail))]
       [(struct-field struct-name:struct-id field-name:id (~optional field-pat #:defaults ([field-pat #'field-name])))
        (define/syntax-parse struct-name? (get-struct-pred-id #'struct-name))
        #'(if (struct-name? target)
              (let* ([field-lens (struct-lens struct-name field-name)]
                     [field-value (lens-get field-lens target)])
                (validate-target field-value field-pat body on-fail))
              (on-fail))]
       [(iso target? forward backward pat)
        #'(if (target? target)
            (let ([new-target (forward target)])
              (validate-target new-target pat body on-fail))
            (on-fail))]
       [(optic target? o pat)
        #'(if (target? target)
              ; this assumes all optics are traversals. This will not be the case if we add folds. but folds are ->list-able
              (let ([new-targets (traversal->list o target)])
                (validate-targets new-targets pat body on-fail))
              (on-fail))]
       [(and2 pat1 pat2)
        #'(validate-target target pat1 (validate-target target pat2 body on-fail) on-fail)]
       [(? predicate)
        #'(if (predicate target)
              body
              (on-fail))]
       [_ #'body]
       [var:id #'body])]))

; helper for validating a sequence of targets with the same pattern
(define-syntax validate-targets
  (syntax-parser
    [(_ targets:id pat body on-fail:id)
     #'(let ([return-false (const #f)])
         (if (and (sequence? targets) (for/and ([target targets]) (validate-target target pat #t return-false)))
             body
             (on-fail)))]))

(define-for-syntax (get-struct-pred-id struct-id-stx)
  (syntax-parse struct-id-stx
    [s:struct-id
     (attribute s.predicate-id)]))

; compose and bind lenses
(define-syntax bind-optics
  (syntax-parser
    [(_ current-optic:id pat body)
     (syntax-parse #'pat
       #:literal-sets (pattern-literals)
       [(cons car-pat cdr-pat)
        #'(let ([optic-with-car (optic-compose current-optic car-lens)]
                [optic-with-cdr (optic-compose current-optic cdr-lens)])
            (bind-optics optic-with-car car-pat
                         (bind-optics optic-with-cdr cdr-pat body)))]
       [(list) #'body]
       [(list pat0 pat ...)
        #'(bind-optics current-optic (cons pat0 (list pat ...)) body)]
       [(list-of pat)
        #'(let ([new-optic (optic-compose current-optic list-traversal)])
            (bind-optics new-optic pat body))]
       [(struct-field struct-name:struct-id field-name:id (~optional field-pat #:defaults ([field-pat #'field-name])))
        #'(let ([new-optic (optic-compose current-optic (struct-lens struct-name field-name))])
            (bind-optics new-optic field-pat body))]
       [(iso target? forward backward pat)
        #'(let ([new-optic (optic-compose current-optic (make-iso forward backward))])
            (bind-optics new-optic pat body))]
       [(optic target? o pat)
        #'(let ([new-optic (optic-compose current-optic o)])
            (bind-optics new-optic pat body))]
       [(and2 pat1 pat2)
        #'(bind-optics current-optic pat1 (bind-optics current-optic pat2 body))]
       [(? predicate (~optional (~seq #:description desc)))
        #'body]
       [_ #'body]
       [var:id
        #'(let ([var current-optic])
            body)])]))


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
               (update 'foo [(iso symbol? symbol->string string->symbol str) (modify str string-upcase)])
               'FOO)
  (test-equal? "can specify an optic directly"
               (update (list 1 2) [(optic list? list-traversal a) (modify a -)])
               (list -1 -2))
  (test-equal? "and pattern"
               (update (list 1 2) [(and2 (list-of n) (list a b))
                                   (modify n -)
                                   (modify a number->string)])
               (list "-1" -2))
  (test-equal? "? pattern"
               (update (list 1 2) [(list (and2 (? odd?) a) (? even?)) (modify a -)])
               (list -1 2))
  (test-equal? "multi-clause"
               (update (list 1 2)
                       [(struct-field posn x) (set x 3)]
                       [(list a b) (set a 4)])
               (list 4 2))
  (test-equal? "use current-update-target"
               (update (list 1 2) [(list a b) (optic-set a (current-update-target) 3)])
               '(3 2)))



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
               (update '(foo bar) [(cons (iso symbol? symbol->string string->symbol str) _) (modify str string-upcase)])
               '(FOO bar))
  ; regression test:
  ; structure check was broken for list-of. structure check only really worked for lenses.
  ; it used to run the nested structure check on the list itself, not each element.
  (test-equal? "list-of structs works"
               (update (list (posn 1 2) (posn 3 4)) [(list-of (struct-field posn x)) (modify x -)])
               (list (posn -1 2) (posn -3 4)))
  (test-equal? "failure in a list-of"
               (update (list 1 2 (posn 3 4))
                       [(list-of (? number?)) 2]
                       [_ 3])
               3))
