#lang racket

(module+ test (require rackunit))
(provide)



(require bindingspec
         "../optics/traversal.rkt"
         "../optics/isomorphism.rkt"
         "../optics/lens.rkt"
         (rename-in "../private/core.rkt"
                    [update core-update])
         (for-syntax syntax/parse syntax/parse/class/struct-id))



(define-hosted-syntaxes

  (binding-class var #:description "pattern variable")
  (extension-class pattern-macro
                   #:description "update pattern macro"
                   #:binding-space pattern-update)

  (nonterminal clause
                [p:pat e:expr]
                #:binding
                (nest-one p e))

  (nesting-nonterminal pat (body)
                       #:bind-literal-set pattern-literals
                       #:allow-extention pattern-macro
                       v:var
                       #:binding {(bind v) body}
                       _
                       #:binding {body}
                       (optic target?:expr o:expr p:pat)
                       #:binding (nest-one p body)
                       (and2 p1:pat p2:pat)
                       #:binding (nest-one p1 (nest-one p2 body))
                       (? pred:expr)
                       #:binding {body}
                       ; TODO literals in pm space so they can be provided, renamed, etc.
                       ; TODO just make these macros!
                       (~> (cons p1 p2)
                           #'(and (optic cons? car-lens p1) (optic cons? cdr-lens p2)))
                       (~> (list)
                           #'_)
                       (~> (list p0 p ...)
                           #'(cons p0 (list p ...)))
                       (~> (list-of p)
                           #'(optic list? list-traversal p))
                       (~> (struct-field struct-name:struct-id field-name:id (~optional field-pat #:defaults ([field-pat #'field-name])))
                           (define/syntax-parse predicate (get-struct-pred-id #'struct-name))
                           #'(optic predicate (struct-field-lens struct-name field-name) field-pat))
                       (~> (iso target? forward backward pat)
                           #'(optic target? (make-iso forward backward) pat))
                       (~> (and)
                           #'_)
                       (~> (and p0 p ...)
                           #'(and2 p0 (and p ...)))
                       (~> (? pred p ...)
                           #'(and (? pred) p ...))))

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

(define-host-interface/expression (update target:expr c:clause)
  #:with compiled-c (compile-clause #'c)
  (println #'compiled-c)
  #'(parameterize ([current-update-target target])
      compiled-c))

;;; left off here. I think I have to do compile-reference in the core compiler, unfortunately
;;; see https://github.com/michaelballantyne/bindingspec/blob/main/test/dsls/minikanren-rs2e/mk.rkt for a full example

(begin-for-syntax
  (define (compile-clause c)
    (syntax-parse c
      [[p body] (compile-pattern #'identity-iso #'p #'body)]))

  (define (compile-pattern current-optic p body)
    (syntax-parse p
      [p
       ;#:with compiled-body (resume-host-expansion body #:reference-compilers ([var compile-reference]))
       #:with body body
       #:with current-optic current-optic
       (syntax-parse #'p
         #:literal-sets (pattern-literals)
         [var:id
          #:with compiled-var (compile-binder! #'var)
          #:with compiled-body (resume-host-expansion #'body #:reference-compilers ([var compile-reference]))
          #'(let ([compiled-var current-optic]) compiled-body)]
         [_
          #:with compiled-body (resume-host-expansion #'body #:reference-compilers ([var compile-reference]))
          #'compiled-body]
         [(optic target? o p)
          #`(let ([new-optic (optic-compose current-optic o)])
              #,(compile-pattern #'new-optic #'p #'compiled-body))]
         [(and2 p1 p2)
          (compile-pattern #'current-optic #'p1 (compile-pattern #'current-optic #'p2 #'compiled-body))]
         [(? predicate) #'compiled-body])])))



(module+ test)
