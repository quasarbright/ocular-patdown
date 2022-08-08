#lang racket

(module+ test (require rackunit))

(provide
 traversal?
 #;(-> (-> (-> focus/c focus/c) target/c target/c)
       (-> target/c (sequence/c focus/c))
       traversal?)
 ; Create a traversal from a mapping function and a function that generates a sequence of all foci
 ; The order of arguments goes against the convention of the lens library, but matches the convention of
 ; map to make the creation of traversals easier.
 ; example:
 #;(define list-map-traversal (make-traversal map identity))
 make-traversal
 ; traversal that focuses on all elements of a list
 list-map-traversal
 #;(-> traversal? target/c (-> focus/c focus/c) target/c)
 ; apply a procedure to each focus and return the updated target
 traversal-modify
 #;(-> traversal? target/c (sequence/c focus/c))
 ; return a sequence of all foci
 traversal->sequence)

(require)

(struct traversal (map to-sequence) #:constructor-name make-traversal)

; traversal that focuses on all elements of a list
(define list-map-traversal (make-traversal map identity))

; traversal that focuses on all elements of a vector
(define vector-map-traversal (make-traversal vector-map identity))

; apply proc to the foci of target under t
(define (traversal-modify t target proc)
  ((traversal-map t) proc target))

(module+ test
  (check-equal? (traversal-modify list-map-traversal '(1 2 3) add1) '(2 3 4)))

; return a sequence of all foci
(define (traversal->sequence t target)
  ((traversal-to-sequence t) target))

(module+ test
  (check-equal? (traversal->sequence list-map-traversal '(1 2 3)) '(1 2 3)))

#;(-> traversal? traversal? traversal?)
; compose two traversals
(define (traversal-compose2 outer-traversal inner-traversal)
  (make-traversal (λ (inner-proc outer-target)
                    (traversal-modify outer-traversal
                                      outer-target
                                      (λ (inner-target)
                                        (traversal-modify inner-traversal inner-target inner-proc))))
                  (λ (outer-target)
                    (for*/stream ([inner-target (traversal->sequence outer-traversal outer-target)]
                                  [inner-focus (traversal->sequence inner-traversal inner-target)])
                      inner-focus))))

(module+ test
  (define lov-traversal (traversal-compose2 list-map-traversal vector-map-traversal))
  (check-equal? (traversal-modify lov-traversal '(#(1) #(2 3)) add1) '(#(2) #(3 4)))
  (check-equal? (sequence->list (traversal->sequence lov-traversal '(#(1) #(2 3)))) '(1 2 3)))

(module+ test)
