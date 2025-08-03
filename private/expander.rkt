#lang racket

(module+ test (require rackunit))
(require syntax-spec-v3
         "../optics/traversal.rkt"
         "../optics/isomorphism.rkt"
         "../optics/lens.rkt"
         "../optics/optic.rkt"
         (for-syntax syntax/parse syntax/transformer syntax/parse/class/struct-id))
(provide #;(update expr [pattern body ...+] ...)
         ; like match, but can also be used for immutably updating values.
         ; Use get, set, modify, etc. on pattern-bound variables.
         ; Under the hood, variables are bound to optics like lenses and traversals
         update
         ; (modify x proc) apply proc to update the foci of x
         modify!
         ; (fold x proc init) foldl over the foci of x
         fold
         optic
         (contract-out
          #;(-> lens? any/c)
          ; get the value
          [get (-> lens? any/c)]
          #;(-> lens? any/c any/c)
          ; set the value. You can also just use set!.
          [optic-set! (-> lens? any/c any/c)])
         ; a parameter for the current update target. Gets updated by set, modify, etc.
         ; Useful for using optics with low-level functions. Ex:
         #;(update (list 1 2) [(list a b) (optic-set a (current-update-target) 3)])
         #;'(3 2)
         ; get, set, modify, etc. use this under the hood.
         ; NOTE: When using this parameter, be careful when performing multiple updates. You must update the parameter
         ; with its new value.
         current-update-target
         (for-space pattern-update
                    ; accepts all values and binds nothing
                    _
                    #;(optic target?:expr o:expr p:pat)
                    ; accepts values which pass the predicate 'target?',
                    ; composes with optic 'o', and continues matching against 'pat'.
                    optic
                    #;(and p:pat ...)
                    ; matches all patterns on the target
                    and
                    #;(? pred:expr)
                    ; ensure the value matches the predicate. fail otherwise.
                    ?
                    #;(cons car-pat:pat cdr-pat:pat)
                    ; matches a cons cell and matches the car against 'car-pat' and the cdr against 'cdr-pat'.
                    cons
                    #;(list p:pat ...)
                    ; matches a list with as many elements as patterns in the list pattern,
                    ; matches each sub-pattern to on its corresponding element.
                    list
                    #;(list-of p:pat)
                    ; matches a list and matches each element against the pattern.
                    ; All variables bound in the pattern operate on all elements.
                    list-of
                    #;(struct-field struct-name:struct-id field-name:id p:pat)
                    ; matches a struct's field
                    struct-field
                    #;(iso target? forward backward p:pat)
                    ; matches a value satisfying the 'target?' predicate and composes the current optic with the specified isomorphism.
                    ; Useful for treating a symbol as a string, for example.
                    iso)
         ; like define-syntax, but defines a macro for patterns.
         define-update-syntax)

(syntax-spec
  (binding-class optic-var #:reference-compiler optic-var-reference-compiler)
  (extension-class pattern-macro
                   #:description "update pattern macro"
                   #:binding-space pattern-update)

  (nonterminal/exporting pat
                        #:description "pattern"
                        #:allow-extension pattern-macro
                        #:binding-space pattern-update
                        v:optic-var
                        #:binding (export v)
                        _
                        ;; ex: (posn [y y^] x)
                        ;; order doesn't matter, field sub-pattern is optional, don't need all fields
                        (~> (name:struct-id field ...)
                            #'(struct* name field ...))
                        (optic target?:racket-expr o:racket-expr p:pat)
                        #:binding (re-export p)
                        (and2 p1:pat p2:pat)
                        #:binding [(re-export p1) (re-export p2)]
                        (#%? pred:racket-expr))

  (host-interface/expression (update* target:racket-expr p:pat body:racket-expr on-fail:racket-expr)
    #:binding (scope (import p) body)
    #'(let ([on-fail-proc (thunk on-fail)]
            [target-v target])
        (validate-target target-v p
                         (bind-optics identity-iso p
                                      (parameterize ([current-update-target target-v])
                                        body))
                         on-fail-proc)))

  (host-interface/expression (optic x:optic-var)
    #'x))

(define-for-syntax (get-struct-pred-id struct-id-stx)
  (syntax-parse struct-id-stx
    [s:struct-id
     (attribute s.predicate-id)]))

(define-syntax-rule
  (define-update-syntax m rhs)
  (define-dsl-syntax m pattern-macro rhs))

(define-update-syntax cons (syntax-rules () [(cons a d) (and (optic cons? car-lens a) (optic cons? cdr-lens d))]))
(define-update-syntax list (syntax-rules () [(list) (? null?)] [(list p0 p ...) (cons p0 (list p ...))]))
(define-update-syntax list-of (syntax-rules () [(listof p) (optic list? list-traversal p)]))
(define-update-syntax struct*
  (syntax-parser
    [(_ struct-name:struct-id field ...)
     #'(and (struct*/help struct-name field) ...)]))
(define-update-syntax struct*/help
  (syntax-parser
    [(_ struct-name:struct-id field-name:id)
     #'(struct-field struct-name field-name field-name)]
    [(_ struct-name:struct-id [field-name:id field-pat])
     #'(struct-field struct-name field-name field-pat)]))
(define-update-syntax struct-field
  (syntax-parser [(_ struct-name:struct-id field-name:id (~optional field-pat #:defaults ([field-pat #'field-name])))
                  (define/syntax-parse predicate (get-struct-pred-id #'struct-name))
                  #'(optic predicate (struct-lens struct-name field-name) field-pat)]))
(define-update-syntax iso (syntax-rules () [(iso target? forward backward pat) (optic target? (make-iso forward backward) pat)]))
(define-update-syntax and (syntax-rules () [(and) _] [(and p0 p ...) (and2 p0 (and p ...))]))
(define-update-syntax ? (syntax-rules () [(? predicate p ...) (and (#%? predicate) p ...)]))

(define-syntax update
  (syntax-parser
    [(_ target [pat body ...] clause ...)
     #'(update* target pat (let () body ...) (update target clause ...))]
    [(_ target)
     #'(error 'update "no matching clause for ~v" target)]))

(begin-for-syntax
  (define-literal-set pattern-literals
    #:datum-literals (cons list list-of struct-field iso optic and2 ? _)
    ())

  (define optic-var-reference-compiler
    (make-variable-like-reference-compiler
     (syntax-parser
       [x:id #'(get x)])
     (syntax-parser
       [(set! x val)
        #'(optic-set! x val)]))))

(define-syntax validate-target
  (syntax-parser
    [(_ target:id pat body on-fail:id)
     (syntax-parse #'pat
       #:literal-sets (pattern-literals)
       [var:id #'body]
       [_ #'body]
       [(optic target? o pat)
        #`(if (target? target)
              (let ([new-targets (traversal->list o target)])
                (validate-targets new-targets pat body on-fail))
              (on-fail))]
       [(and2 p1 p2)
        #'(validate-target target p1
                           (validate-target target p2 body on-fail)
                           on-fail)]
       [(#%? predicate)
        #'(if (predicate target)
              body
              (on-fail))])]))

(define-syntax validate-targets
  (syntax-parser
    [(_ targets:id pat body on-fail:id)
     #'(let ([return-false (const #f)])
           (if (and (sequence? targets)
                    (for/and ([target targets])
                      (validate-target target pat #t return-false)))
               body
               (on-fail)))]))

(define-syntax bind-optics
  (syntax-parser
    [(_ current-optic:id p body)
     (syntax-parse #'p
       #:literal-sets (pattern-literals)
       [var:id
        #'(let ([var current-optic])
            body)]
       [_
        #'body]
       [(optic target? o p)
        #'(let ([new-optic (optic-compose current-optic o)])
            (bind-optics new-optic p body))]
       [(and2 p1 p2)
        #'(bind-optics current-optic p1 (bind-optics current-optic p2 body))]
       [(#%? predicate)
        #'body])]))

; the scrutinee of the current update form. Does not change as the update dives into the structure.
(define current-update-target (make-parameter #f #f 'current-update-target-not-initialized))
; retrieve the focus of the current target under 'optic'
(define (get optic) (optic-get optic (current-update-target)))
; set the focus of the current target under 'optic'
; don't use this. use set!
(define (optic-set! optic focus)
  (current-update-target (optic-set optic (current-update-target) focus))
  (current-update-target))
; apply a function to update the focus of the current target under 'optic'
(define-syntax-rule (modify! x func) (modify!/proc (optic x) func))
(define (modify!/proc optic func)
  (current-update-target (traversal-map optic (current-update-target) func))
  (current-update-target))
; fold over the current target's foci under 'traversal'
(define-syntax-rule (fold x proc init) (fold/proc (optic x) proc init))
(define (fold/proc traversal proc init) (traversal-foldl traversal (current-update-target) proc init))

(module+ test
  ; you can set values
  (check-equal? (update (list 1 2) [(list a b) (set! a #t) (set! b #f)])
                (list #t #f))
  ; you can set values with set!
  (check-equal? (update (list 1 2) [(list a b) (set! a #t) (set! b #f)])
                (list #t #f))
  ; you can apply functions to modify values
  (check-equal? (update (list 1 2) [(list a b) (modify! b add1)])
                '(1 3))
  ; you can nest patterns to perform deep updates
  (check-equal? (update '(1 (2 3)) [(list a (list b c)) (set! c #t)])
                '(1 (2 #t)))
  ; get, set, etc. are procedures and can be used in any expression position inside of an update form
  (check-equal? (update (list 7 8 9)
                        [(list a b c)
                         (set! a 1)
                         (set! b a)
                         (define x c)
                         (set! c (add1 x))])
                (list 1 1 10))
  ; struct pattern
  (struct posn [x y] #:transparent)
  (check-equal? (update (posn 1 2) [(struct-field posn x a) (set! a 3)]) (posn 3 2))
  ; without a pattern after the field name, the field's optic is bound to the name of the field
  (check-equal? (update (posn 1 2) [(struct-field posn x) (set! x 3)]) (posn 3 2))
  (check-equal? (update (posn 1 2) [(posn [x a]) (set! a 3)])
                (posn 3 2))
  (check-equal? (update (posn 1 2) [(posn x) (set! x 3)])
                (posn 3 2))
  ; list-of pattern creates a traversal which can modify all elements
  (check-equal? (update (list 1 2 3 4) [(list-of a) (modify! a -)]) '(-1 -2 -3 -4))
  ; you can fold(l) the elements of a traversal
  (check-equal? (update (list 1 2 3) [(list-of a) (fold a cons '())])
                (list 3 2 1))
  ; if the last expression of the body is a get, its value is returned
  (check-equal? (update '(1 2) [(list a b) a]) 1)
  ; you can access the optic directly
  (check-pred lens? (update '(1 2) [(list a b) (optic a)]))
  (test-equal? "can use iso to treat an X as a Y"
               (update 'foo [(iso symbol? symbol->string string->symbol str) (modify! str string-upcase)])
               'FOO)
  (test-equal? "can specify an optic directly"
               (update (list 1 2) [(optic list? list-traversal a) (modify! a -)])
               (list -1 -2))
  (test-equal? "and pattern"
               (update (list 1 2) [(and2 (list-of n) (list a b))
                                   (modify! n -)
                                   (modify! a number->string)])
               (list "-1" -2))
  (test-equal? "? pattern"
               (update (list 1 2) [(list (and2 (? odd?) a) (? even?)) (modify! a -)])
               (list -1 2))
  (test-equal? "multi-clause"
               (update (list 1 2)
                       [(struct-field posn x) (set! x 3)]
                       [(list a b) (set! a 4)])
               (list 4 2))
  (test-equal? "use current-update-target"
               (update (list 1 2) [(list a b) (optic-set (optic a) (current-update-target) 3)])
               '(3 2))
  (check-equal? (update (list 1 2) [(cons a (cons b _)) (set! a #t) (set! b #f)])
                (list #t #f))
  (check-equal? (update (list 1 2) [(list a b) (modify! a -) (set! b #t)]) (list -1 #t))
  (check-equal? (update (posn (cons 1 2) 3) [(struct-field posn x (cons a b)) (modify! a -) (modify! b sqr)]) (posn (cons -1 4) 3))
  (check-equal? (update '((1 2) (3 4) (5 6)) [(list-of (list a _)) (modify! a -)]) '((-1 2) (-3 4) (-5 6)))
  (check-equal? (update '(((1 2) (3)) ((4) ())) [(list-of (list-of (list-of a))) (modify! a -)]) '(((-1 -2) (-3)) ((-4) ())))
  (test-equal? "update in an update works"
               (update '(1 (2 3)) [(list a b)
                                    (set! b (update b [(list c d) (set! d 4)]))
                                    (set! a #t)])
                '(#t (2 4)))
  (test-equal? "iso composes"
               (update '(foo bar) [(cons (iso symbol? symbol->string string->symbol str) _) (modify! str string-upcase)])
               '(FOO bar))
  ; regression test:
  ; structure check was broken for list-of. structure check only really worked for lenses.
  ; it used to run the nested structure check on the list itself, not each element.
  (test-equal? "list-of structs works"
               (update (list (posn 1 2) (posn 3 4)) [(list-of (struct-field posn x)) (modify! x -)])
               (list (posn -1 2) (posn -3 4)))
  (test-equal? "failure in a list-of"
               (update (list 1 2 (posn 3 4))
                       [(list-of (? number?)) 2]
                       [_ 3])
               3)
  (check-equal? (update 1 [_ 2]) 2)
  (check-equal? (update 1 [a a])
                1)
  (check-equal? (update '(1) [(optic cons? car-lens a) a]) 1)
  (check-equal? (update '(1 2) [(and a (cons b c)) (list a b c)]) '((1 2) 1 (2)))
  (check-equal? (update '(1 2) [(list a b) b]) 2)
  (test-equal? "multi-clause"
                (update '(1 2)
                        [(list a b c) (error "boom")]
                        [(list a b) (list b a)])
                '(2 1))
  ; regression test: (list) used to expand to _, rather than asserting the target is null.
  (test-equal? "list pattern with too few elements fails"
                (update '(1 2 3)
                        [(list a b) (error "boom")]
                        [(list a b c) (list b a)])
                '(2 1))
  (test-equal? "implicit begin in clause"
               (update 1 [a (define x a) x])
               1))
