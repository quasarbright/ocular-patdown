#lang racket

;; a smaller version of the update language to demonstrate at a lecture

(module+ test (require rackunit))
(require syntax-spec-v3
         "../../optics/traversal.rkt"
         "../../optics/isomorphism.rkt"
         "../../optics/lens.rkt"
         "../../optics/optic.rkt"
         (for-syntax syntax/parse syntax/transformer syntax/parse/class/struct-id))

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
    (optic o:racket-expr p:pat)
    #:binding (re-export p)
    (and2 p1:pat p2:pat)
    #:binding [(re-export p1) (re-export p2)])

  (host-interface/expression
    (update target:racket-expr [p:pat body:racket-body ...])
    #:binding (scope (import p) (import body) ...)
    #'(let ([target-v target])
        (bind-optics identity-iso p
                     (parameterize ([current-update-target target-v])
                       body
                       ...)))))

(define-syntax-rule
  (define-update-syntax m rhs)
  (define-dsl-syntax m pattern-macro rhs))

(define-update-syntax cons (syntax-rules () [(cons a d) (and (optic car-lens a) (optic cdr-lens d))]))
(define-update-syntax list (syntax-rules () [(list) _] [(list p0 p ...) (cons p0 (list p ...))]))
(define-update-syntax list-of (syntax-rules () [(listof p) (optic list-traversal p)]))
(define-update-syntax struct-field
  (syntax-parser [(_ struct-name:struct-id field-name:id (~optional field-pat #:defaults ([field-pat #'field-name])))
                  #'(optic (struct-lens struct-name field-name) field-pat)]))
(define-update-syntax iso (syntax-rules () [(iso forward backward pat) (optic (make-iso forward backward) pat)]))
(define-update-syntax and (syntax-rules () [(and) _] [(and p0 p ...) (and2 p0 (and p ...))]))

(begin-for-syntax
  (define-literal-set pattern-literals
    #:datum-literals (optic and2 _)
    ())

  (define optic-var-reference-compiler
    (make-variable-like-reference-compiler
     (lambda (id) id)
     (syntax-parser
       [(set! id val)
        #'(optic-set! id val)]))))

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
       [(optic o p)
        #'(let ([new-optic (optic-compose current-optic o)])
            (bind-optics new-optic p body))]
       [(and2 p1 p2)
        #'(bind-optics current-optic p1 (bind-optics current-optic p2 body))])]))

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
(define (modify! optic func)
  (current-update-target (traversal-map optic (current-update-target) func))
  (current-update-target))
; fold over the current target's foci under 'traversal'
(define (fold traversal proc init) (traversal-foldl traversal (current-update-target) proc init))

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
                         (set! b (get a))
                         (let ([x (get c)])
                           (set! c (add1 x)))])
                (list 1 1 10))
  ; struct pattern
  (struct posn [x y] #:transparent)
  (check-equal? (update (posn 1 2) [(struct-field posn x a) (set! a 3)]) (posn 3 2))
  ; without a pattern after the field name, the field's optic is bound to the name of the field
  (check-equal? (update (posn 1 2) [(struct-field posn x) (set! x 3)]) (posn 3 2))
  ; list-of pattern creates a traversal which can modify all elements
  (check-equal? (update (list 1 2 3 4) [(list-of a) (modify! a -)]) '(-1 -2 -3 -4))
  ; you can fold(l) the elements of a traversal
  (check-equal? (update (list 1 2 3) [(list-of a) (fold a cons '())])
                (list 3 2 1))
  ; if the last expression of the body is a get, its value is returned
  (check-equal? (update '(1 2) [(list a b) (get a)]) 1)
  ; you can access the optic directly
  (check-pred lens? (update '(1 2) [(list a b) a]))
  (test-equal? "can use iso to treat an X as a Y"
               (update 'foo [(iso symbol->string string->symbol str) (modify! str string-upcase)])
               'FOO)
  (test-equal? "can specify an optic directly"
               (update (list 1 2) [(optic list-traversal a) (modify! a -)])
               (list -1 -2))
  (test-equal? "and pattern"
               (update (list 1 2) [(and2 (list-of n) (list a b))
                                   (modify! n -)
                                   (modify! a number->string)])
               (list "-1" -2))
  (test-equal? "use current-update-target"
               (update (list 1 2) [(list a b) (optic-set a (current-update-target) 3)])
               '(3 2))
  (check-equal? (update (list 1 2) [(cons a (cons b _)) (set! a #t) (set! b #f)])
                (list #t #f))
  (check-equal? (update (list 1 2) [(list a b) (modify! a -) (set! b #t)]) (list -1 #t))
  (check-equal? (update (posn (cons 1 2) 3) [(struct-field posn x (cons a b)) (modify! a -) (modify! b sqr)]) (posn (cons -1 4) 3))
  (check-equal? (update '((1 2) (3 4) (5 6)) [(list-of (list a _)) (modify! a -)]) '((-1 2) (-3 4) (-5 6)))
  (check-equal? (update '(((1 2) (3)) ((4) ())) [(list-of (list-of (list-of a))) (modify! a -)]) '(((-1 -2) (-3)) ((-4) ())))
  (test-equal? "update in an update works"
               (update '(1 (2 3)) [(list a b)
                                   (set! b (update (get b) [(list c d) (set! d 4)]))
                                   (set! a #t)])
               '(#t (2 4)))
  (test-equal? "iso composes"
               (update '(foo bar) [(cons (iso symbol->string string->symbol str) _) (modify! str string-upcase)])
               '(FOO bar))
  ; regression test:
  ; structure check was broken for list-of. structure check only really worked for lenses.
  ; it used to run the nested structure check on the list itself, not each element.
  (test-equal? "list-of structs works"
               (update (list (posn 1 2) (posn 3 4)) [(list-of (struct-field posn x)) (modify! x -)])
               (list (posn -1 2) (posn -3 4)))
  (check-equal? (update 1 [_ 2]) 2)
  (check-pred optic? (update 1 [a a]))
  (check-equal? (update '(1) [(optic car-lens a) (get a)]) 1)
  (check-equal? (update '(1 2) [(and a (cons b c)) (map get (list a b c))]) '((1 2) 1 (2)))
  (check-equal? (update '(1 2) [(list a b) (get b)]) 2)
  (test-equal? "define in clause"
               (update (list 1 2)
                 [(list a b)
                  (define a^ (set! a (add1 (get a))))
                  (set! a a^)])
               (list (list 2 2) 2)))
