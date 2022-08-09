#lang racket

; provides a general optic compose which uses the most specific composition operator
; possible and aliases for get, set, and modify

(module+ test (require rackunit))
(provide
 optic?
 optic-compose
 optic-get
 optic-set
 optic-modify)



(require "./lens.rkt" "./isomorphism.rkt" "./traversal.rkt")



(define optic? traversal?)

(define (optic-compose . optics)
  (cond
    [(andmap iso? optics) (apply iso-compose optics)]
    [(andmap lens? optics) (apply lens-compose optics)]
    [(andmap traversal? optics) (apply traversal-compose optics)]))

(module+ test
  (check-pred iso? (iso-compose symbol<->string))
  (check-pred (conjoin (negate iso?) lens?) (optic-compose cdr-lens car-lens))
  (check-equal? (lens-get (optic-compose cdr-lens car-lens) '(1 2)) 2)
  (check-equal? (traversal-modify (optic-compose list-traversal symbol<->string)
                                  '(foo bar)
                                  string-upcase)
                '(FOO BAR)))

(define optic-get lens-get)
(define optic-set lens-set)
(define optic-modify traversal-modify)



(module+ test
  (check-equal? (optic-modify (optic-compose list-traversal car-lens symbol<->string)
                                  '((foo bar) (baz bang quux))
                                  string-upcase)
                '((FOO bar) (BAZ bang quux))))
