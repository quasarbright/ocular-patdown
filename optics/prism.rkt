#lang racket

#|
A prism is like an isomorphism, but may only work in one direction.
An isomorphism is used when two types are equivalent. A prism is used when one type
is a "subtype" of another.
All isomorphisms are prisms, but not all prisms are isomorphisms.
A prism is not the same thing as a lens which may not have a focus. A prism must also be able
to create the target from the focus alone.
|#

(module+ test (require rackunit))
(provide
 gen:prism
 prism?
 prism-absent
 prism-absent?
 #;(-> (-> target/c (or/c focus/c prism-absent?))
       (-> focus/c target/c)
       prism?)
 string-number-prism
 ; Create a prism from a projector and an injector
 ; The projector may return (prism-absent) if the focus is not present.
 (contract-out
  [make-prism (-> any/c (or/c any/c prism-absent?) any/c)]
  [guard-prism (-> (-> any/c any/c) prism?)]
  [prism-project (->* (prism? any/c) ((or/c any/c (-> any/c))) any/c)]
  [prism-inject (-> prism? any/c any/c)]
  [prism-compose (->* () #:rest (listof prism?) prism?)]))

(require racket/generic
         "./traversal.rkt")



(define-generics prism
  (prism-project prism target [failure-result])
  (prism-inject prism focus))

(struct prism-absent [] #:transparent)
(define default-failure-result (λ () (error 'prism-project "focus not present in prism")))

(struct make-prism (project inject)
  #:methods gen:prism
  [(define (prism-project prism target [failure-result default-failure-result])
     (let ([focus ((make-prism-project prism) target)])
       (if (prism-absent? focus)
           (if (procedure? failure-result)
               (failure-result)
               failure-result)
           focus)))

   (define (prism-inject prism focus)
     ((make-prism-inject prism) focus))]

  #:methods gen:traversal
  [(define (traversal-map prism target proc)
     (prism-match prism target
                  [focus (prism-inject prism (proc focus))]
                  [target]))
   (define (traversal-foldl prism target proc init)
     (prism-match prism target
                  [focus (proc focus init)]
                  [init]))])

(define string-number-prism
  (make-prism (λ (s) (or (string->number s) (prism-absent)))
              number->string))

(define singleton-prism
  (make-prism (λ (l) (match l
                       [(list a) a]
                       [_ (prism-absent)]))
              (λ (v) (list v))))

; notice how square isn't literally a sub-type of rectangle (in the sense of struct inheritance).
(struct square [length] #:transparent)
(struct rectangle [width height] #:transparent)
(define rectangle-square-prism
  (make-prism (λ (rect)
                (if (= (rectangle-width rect) (rectangle-height rect))
                    (square (rectangle-height rect))
                    (prism-absent)))
              (λ (sqr) (rectangle (square-length sqr) (square-length sqr)))))

(define (guard-prism predicate) (make-prism (λ (target) (if (predicate target) target (prism-absent)))
                                            ; TODO guarded injector with contract error?
                                            identity))

(define myzero '())
(define (myzero? n) (null? n))
(define (myadd1 n) (list n))
(define (mysub1 n) (car n))
(define (nat->mynat n) (if (zero? n) myzero (myadd1 (nat->mynat (sub1 n)))))
(define (mynat->nat n) (if (myzero? n) 0 (add1 (mynat->nat (mysub1 n)))))
(define mynat-prism (make-prism (λ (num) (if (natural? num) (nat->mynat num) (prism-absent)))
                                mynat->nat))

(define identity-prism (make-prism identity identity))

(module+ test
  (check-equal? (prism-project string-number-prism "1") 1)
  (check-equal? (prism-project string-number-prism "foo" (prism-absent)) (prism-absent))
  (check-equal? (prism-inject string-number-prism 1) "1")
  (check-equal? (prism-project mynat-prism 1) '(()))
  (check-equal? (prism-project mynat-prism -1 (prism-absent)) (prism-absent))
  (check-equal? (prism-inject mynat-prism '(())) 1)
  (check-equal? (prism-project rectangle-square-prism (rectangle 1 1)) (square 1))
  (check-equal? (prism-project rectangle-square-prism (rectangle 1 3) (prism-absent)) (prism-absent))
  (check-equal? (prism-inject rectangle-square-prism (square 2)) (rectangle 2 2)))



(define (prism-project-cont prism target on-success failure-result)
  (let ([focus (prism-project prism target prism-absent)])
    (if (prism-absent? focus)
        (if (procedure? failure-result)
            (failure-result)
            failure-result)
        (on-success focus))))

; TODO make this a more strict macro
(define-syntax-rule
  (prism-match prism target [focus-id on-success ...] [on-fail ...])
  (prism-project-cont prism target (λ (focus-id) on-success ...) (λ () on-fail ...)))

(module+ test
  (check-equal? (prism-match string-number-prism "1" [n (add1 n)] ['nan]) 2)
  (check-equal? (prism-match string-number-prism "foo" [n (add1 n)] ['nan]) 'nan))

(define (prism-compose2 outer-prism inner-prism)
  (make-prism (λ (outer-target)
                (prism-match outer-prism outer-target
                             [inner-target (prism-project inner-prism inner-target (prism-absent))]
                             [(prism-absent)]))
              (λ (inner-focus) (prism-inject outer-prism (prism-inject inner-prism inner-focus)))))

(module+ test
  (define string-mynat-prism (prism-compose2 string-number-prism mynat-prism))
  (check-equal? (prism-project string-mynat-prism "1") '(()))
  (check-equal? (prism-project string-mynat-prism "-1" (prism-absent)) (prism-absent))
  (check-equal? (prism-project string-mynat-prism "foo" (prism-absent)) (prism-absent))
  (check-equal? (prism-inject string-mynat-prism '((()))) "2"))

(define (prism-compose . prisms)
  (foldr prism-compose2 identity-prism prisms))

(module+ test
  (define string-posint-mynat-prism (prism-compose string-number-prism (guard-prism positive?) mynat-prism))
  (check-equal? (prism-project string-posint-mynat-prism "1") '(()))
  (check-equal? (prism-project string-posint-mynat-prism "0" (prism-absent)) (prism-absent))
  (check-equal? (prism-inject string-posint-mynat-prism '(())) "1"))



(module+ test)
