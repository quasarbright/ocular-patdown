#lang racket

(module+ test (require rackunit))

(provide
 ; generic interface for traversal
 gen:traversal
 ; traversal predicate
 traversal?
 #;(-> (-> (-> focus/c focus/c) target/c target/c)
       (-> (-> focus/c any/c any/c) any/c target/c any/c)
       traversal?)
 ; Create a traversal from a mapping function and a function that generates a sequence of all foci
 ; The order of arguments goes against the convention of the lens library, but matches the convention of
 ; map to make the creation of traversals easier.
 ; example:
 #;(define list-map-traversal (make-traversal map foldl))
 make-traversal
 ; traversal that focuses on all elements of a list.
 list-traversal
 ; traversal that focuses on all elements of a vector.
 vector-traversal
 ; traversal that focuses on the leaves of a rose tree.
 ; A rose-tree is a non-list or a (listof rose-tree)
 rose-traversal
 ; A traversal that focuses on the target if it is not #f, and has no focus otherwise.
 maybe-traversal
 #;(-> traversal? target/c (-> focus/c focus/c) target/c)
 ; apply a procedure to each focus and return the updated target
 traversal-modify
 #;(-> traversal? target/c (-> focus/c any/c any/c) any/c any/c)
 ; foldl over the target's foci. Unlike list foldl, the target comes first to conform to the library's convention
 traversal-foldl
 ; compose traversals. deepest/innermost traversal last.
 traversal-compose
 #;(-> traversal? target? (listof focus?))
 ; get a list of all foci
 traversal->list)



(require racket/generic racket/generator)



(define-generics traversal
  (traversal-modify traversal target proc)
  (traversal-foldl traversal target proc init))

(struct make-traversal (map foldl)
  #:methods gen:traversal
  [(define (traversal-modify t target proc)
     ((make-traversal-map t) proc target))
   (define (traversal-foldl t target proc init)
     ((make-traversal-foldl t) proc init target))])

; traversal that focuses on all elements of a list
(define list-traversal (make-traversal map foldl))

; traversal that focuses on all elements of a vector
(define vector-traversal (make-traversal vector-map
                                             (λ (proc init v) (for/fold ([acc init]) ([ele v]) (proc ele acc)))))

; traversal that focuses on the target itself
(define identity-traversal (make-traversal (λ (proc v) (proc v)) (λ (proc init v) (proc v init))))

#;(-> (-> any/c any/c) rose? rose?)
; map over the leaves of a rose tree.
(define (rose-map proc rose)
  (if (list? rose)
      (map (λ (child) (rose-map proc child)) rose)
      (proc rose)))

#;(-> (-> any/c any/c any/c) any/c rose? any/c)
; foldl over the leaves of the rose tree.
(define (rose-foldl proc init rose)
  (for/fold ([acc init])
            ([element (rose->sequence rose)])
    (proc element acc)))

#;(-> (rose-of A) (sequence/c A))
; iterate the leaves of the rose tree.
(define (rose->sequence rose)
  (in-generator
   (let loop ([rose rose])
     (if (list? rose)
         (for ([child rose]) (loop child))
         (yield rose)))))

(module+ test
  (check-equal? (sequence->list (rose->sequence '((1 2) () (((((3)) 4))))))
                '(1 2 3 4)))

; traversal that focuses on the leaves of a rose-tree.
; A rose is a non-list or a (listof rose).
(define rose-traversal (make-traversal rose-map rose-foldl))

; traversal that focuses on the target if it is not #f. Has no focus otherwise.
(define maybe-traversal
      (make-traversal
        (lambda (proc val) (if val (proc val) val))
        (lambda (proc init val) (if val (proc val init) init))))

(module+ test
  (check-equal? (traversal-modify list-traversal '(1 2 3) add1) '(2 3 4))
  (check-equal? (traversal-foldl list-traversal '(1 2 3) cons '()) '(3 2 1))
  (check-equal? (traversal-modify vector-traversal #(1 2 3) add1) #(2 3 4))
  (check-equal? (traversal-foldl vector-traversal #(1 2 3) cons '()) '(3 2 1))
  (check-equal? (traversal-modify identity-traversal 1 add1) 2)
  (check-equal? (traversal-foldl identity-traversal 1 cons '()) '(1))
  (check-equal? (traversal-modify rose-traversal '((1) ((2 3) 4)) add1)
                '((2) ((3 4) 5)))
  (check-equal? (traversal->list rose-traversal '((1) ((2 3) 4))) '(1 2 3 4))
  (check-equal? (traversal-modify maybe-traversal 1 add1) 2)
  (check-equal? (traversal-modify maybe-traversal #f add1) #f)
  (check-equal? (traversal-foldl maybe-traversal 1 + 0) 1)
  (check-equal? (traversal-foldl maybe-traversal #f + 0) 0))



#;(-> traversal? traversal? traversal?)
; compose two traversals
(define (traversal-compose2 outer-traversal inner-traversal)
  (make-traversal (λ (inner-proc outer-target)
                    (traversal-modify outer-traversal
                                      outer-target
                                      (λ (inner-target)
                                        (traversal-modify inner-traversal inner-target inner-proc))))
                  (λ (proc init outer-target)
                    (traversal-foldl outer-traversal outer-target
                                     (λ (inner-target outer-acc)
                                       (traversal-foldl inner-traversal inner-target proc outer-acc))
                                     init))))

(module+ test
  (define lov-traversal (traversal-compose2 list-traversal vector-traversal))
  (check-equal? (traversal-modify lov-traversal '(#(1) #(2 3)) add1) '(#(2) #(3 4)))
  (check-equal? (traversal-foldl lov-traversal '(#(1) #(2 3)) cons '()) '(3 2 1)))

#;(-> traversal? ... traversal?)
; compose traversals
(define (traversal-compose . traversals)
  (foldr traversal-compose2 identity-traversal traversals))

(module+ test
  (check-equal? (traversal-modify (traversal-compose list-traversal vector-traversal)
                                  '(#(1))
                                  add1)
                '(#(2))))

#;(-> traversal? target? (listof focus?))
; get a list of all foci
(define (traversal->list traversal target)
  (reverse (traversal-foldl traversal target cons '())))

(module+ test
  (test-equal? "traversal->list"
               (traversal->list (traversal-compose vector-traversal vector-traversal) #(#(1 2 3) #(4 5) #() #(6)))
               '(1 2 3 4 5 6)))



(module+ test
  (define lolol-traversal (traversal-compose list-traversal list-traversal list-traversal))
  (define lolol '(((1 2) (3)) ((4)) () (()())))
  (check-equal? (traversal-modify lolol-traversal lolol add1) '(((2 3) (4)) ((5)) () (()()))))
