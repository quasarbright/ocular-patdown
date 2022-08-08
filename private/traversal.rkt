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
 traversal-modify)

(require)

(struct traversal (map to-sequence) #:constructor-name make-traversal)

; traversal that focuses on all elements of a list
(define list-map-traversal (make-traversal map identity))

; apply proc to the foci of target under t
(define (traversal-modify t target proc)
  ((traversal-map t) proc target))

(module+ test
  (check-equal? (traversal-modify list-map-traversal '(1 2 3) add1)))

(module+ test)
