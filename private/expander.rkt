#lang racket

(module+ test (require rackunit))
(provide (all-defined-out))



(require bindingspec
         "../optics/traversal.rkt"
         "../optics/isomorphism.rkt"
         "../optics/lens.rkt"
         "../optics/optic.rkt"
         (rename-in "../private/core.rkt"
                    [update core-update])
         (for-syntax syntax/parse syntax/parse/class/struct-id))



(define-hosted-syntaxes

  (binding-class var #:description "pattern variable")
  (extension-class pattern-macro
                   #:description "update pattern macro"
                   #:binding-space pattern-update)

  (nonterminal clause
               #:description "matching clause"
                [p:pat e:expr]
                #:binding
                ; this extra scope is a hack to prevent use-site scopes.
                ; the racket expander doesn't add a use-site scope if the use-site is in a separate scope from the macro definition.
                ; if there were use-site scopes added, bindings in macro patterns like (cons a d) wouldn't be in scope in expression positions.
                ; using recursive, export, and re-export might also work.
                {(nest-one p (host e))})

  (nesting-nonterminal pat (body)
                       #:description "pattern"
                       #:bind-literal-set pattern-literals
                       #:allow-extension pattern-macro
                       v:var
                       #:binding {(bind v) body}
                       _
                       #:binding {body}
                       (optic target?:expr o:expr p:pat)
                       #:binding [(nest-one p body) (host target?) (host o)]
                       (and2 p1:pat p2:pat)
                       #:binding (nest-one p1 (nest-one p2 body))
                       (#%? pred:expr)
                       #:binding [{body} (host pred)]))

(define-for-syntax (get-struct-pred-id struct-id-stx)
  (syntax-parse struct-id-stx
    [s:struct-id
     (attribute s.predicate-id)]))

(define-syntax define-update-syntax
  (syntax-parser
    [(_ name:id rhs:expr)
     #:with spaced-name ((make-interned-syntax-introducer 'pattern-update) (attribute name) 'add)
     #'(define-syntax spaced-name rhs)]))

(define-syntax define-pattern-syntax
  (syntax-parser
    [(_ (macro-name:id args ...) body ...) #'(define-pattern-syntax macro-name (λ (args ...) body ...))]
    [(_ macro-name:id transformer) #'(define-update-syntax macro-name (pattern-macro transformer))]))

(define-pattern-syntax cons (syntax-rules () [(cons a d) (and (optic cons? car-lens a) (optic cons? cdr-lens d))]))
(define-pattern-syntax list (syntax-rules () [(list) _] [(list p0 p ...) (cons p0 (list p ...))]))
(define-pattern-syntax list-of (syntax-rules () [(listof p) (optic list? list-traversal p)]))
(define-pattern-syntax struct-field
  (syntax-parser [(_ struct-name:struct-id field-name:id (~optional field-pat #:defaults ([field-pat #'field-name])))
                  (define/syntax-parse predicate (get-struct-pred-id #'struct-name))
                  #'(optic predicate (struct-field-lens struct-name field-name) field-pat)]))
(define-pattern-syntax iso (syntax-rules () [(iso target? forward backward pat) (optic target? (make-iso forward-backward) pat)]))
(define-pattern-syntax and (syntax-rules () [(and) _] [(and p0 p ...) (and2 p0 (and p ...))]))
(define-pattern-syntax ? (syntax-rules () [(? predicate p ...) (and (? predicate) p ...)]))

(define-host-interface/expression (update target:expr c:clause)
  #:with compiled-c (compile-clause #'c)
  #'(parameterize ([current-update-target target])
      compiled-c))

(begin-for-syntax
  (define (compile-clause c)
    (syntax-parse c
      [[p body] (compile-pattern #'identity-iso #'p (compile-host-expr #'body))]))

  (define (compile-pattern current-optic p body)
    (syntax-parse p
      [p
       #:with body body
       #:with current-optic current-optic
       (syntax-parse #'p
         #:literal-sets (pattern-literals)
         [var:id
          #:with compiled-var (compile-binder! #'var)
          #'(let ([compiled-var current-optic]) body)]
         [_
          #'body]
         [(optic target? o p)
          #`(let ([new-optic (optic-compose current-optic #,(compile-host-expr #'o))])
              #,(compile-pattern #'new-optic #'p #'body))]
         [(and2 p1 p2)
          (compile-pattern #'current-optic #'p1 (compile-pattern #'current-optic #'p2 #'body))]
         [(#%? predicate)
          #'body])]))

  (define (compile-host-expr e) (resume-host-expansion e #:reference-compilers ([var compile-reference]))))


(module+ test
  (check-equal? (update 1 [_ 2]) 2)
  (check-pred optic? (update 1 [a a]))
  (check-equal? (update '(1) [(optic cons? car-lens a) (get a)]) 1)
  (check-equal? (update '(1 2) [(and a (cons b c)) (map get (list a b c))]) '((1 2) 1 (2)))
  (check-equal? (update '(1 2) [(list a b) (get b)]) 2))
