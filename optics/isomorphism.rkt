#lang racket

#|
An isomporphism is like a reversible lens. In an isomorphism, the target and focus
are equivalent. For example, lists and vectors are said to be isomorphic. You can convert
a list to a vector and you can convert a vector to a list. They hold equivalent information.

Isomorphisms are useful when you want to treat one type of data as another type. For example,
let's say you want to convert a symbol to uppercase. Here is one approach:

>(string->symbol (string-upcase (symbol->string 'foo)))
'FOO

This is cumbersome. Isomorphisms make this cleaner:

>(define symbol<->string (make-iso symbol->string string->symbol))
>(iso-modify symbol<->string 'foo string-upcase)
'FOO

iso-modify handles the conversions so you can focus on the interesting stuff.
|#

(provide
 ; generic interface for isomorphisms
 gen:iso
 ; isomorphism predicate
 iso?
 (contract-out
  #;(-> (-> target/c focus/c) (-> focus/c target/c) iso?)
  ; Create an isomorphism from two functions that are inverses of each other. Ex:
  #; (define symbol<->string symbol->string string->symbol)
  ; In general, you want the type that you're working with on the left and
  ; the type you want to want to treat it like on the right. In symbol<->string, we have symbols
  ; and want to treat them like strings.
  ; The target is the symbol and the focus is the string.
  [make-iso (-> (-> any/c any/c) (-> any/c any/c) iso?)]
  #;(-> iso? target/c focus/c)
  ; convert target to focus
  [iso-forward (-> iso? any/c any/c)]
  #;(-> iso? focus/c target/c)
  ; convert focus to target
  [iso-backward (-> iso? any/c any/c)]
  #;(-> iso? target/c (-> focus/c focus/c) target/c)
  ; apply a function to the iso's focus and return the modified target. Ex:
  #; (iso-modify symbol<->string 'foo string-upcase)
  #; 'FOO
  ; Useful for treating a target like a focus. But be careful! The procedure must return a new focus,
  ; not just any value.
  ; Alias for traversal-map
  [iso-modify (-> iso? any/c (-> any/c any/c) any/c)]
  #;(-> iso? iso?)
  ; create a new iso with swapped forward and backward converters.
  [iso-reverse (-> iso? iso?)]
  #;(-> iso? ... iso?)
  ; compose isomorphisms. Ex:
  #;(define a<->c (iso-compose a<->b b<->c))
  [iso-compose (->* () #:rest (listof iso?) iso?)])
 ; isomorphism where the target is a symbol and the focus is a string.
 symbol<->string
 ; isomorphism where the target is a vector and the focus is a list.
 vector<->list
 ; isomorphism where the target is a list and the focus is the reversed list.
 list-reverse-iso
 ; isomorphism where the target is the focus.
 identity-iso)
(module+ test (require rackunit))



(require racket/generic
         "./traversal.rkt"
         "./lens.rkt"
         "./prism.rkt")



(define-generics iso
  (iso-forward iso target)
  (iso-backward iso focus))

(struct make-iso [forward backward]
  #:methods gen:iso
  [(define (iso-forward iso target) ((make-iso-forward iso) target))
   (define (iso-backward iso focus) ((make-iso-backward iso) focus))]
  #:methods gen:lens
  [(define (lens-get iso target)
     (iso-forward iso target))
   (define (lens-set iso target focus)
     (iso-backward iso focus))]
  #:methods gen:prism
  [(define (prism-project iso target [_ #f]) (iso-forward iso target))
   (define (prism-inject iso focus) (iso-backward iso focus))]
  #:methods gen:traversal
  [(define (traversal-map iso target proc)
     (iso-backward iso (proc (iso-forward iso target))))
   (define (traversal-foldl iso target proc init)
     (proc (iso-forward iso target) init))])

(define list<->vector (make-iso list->vector vector->list))
(define vector<->list (make-iso vector->list list->vector))
(define symbol<->string (make-iso symbol->string string->symbol))
(define list-reverse-iso (make-iso reverse reverse))
(define identity-iso (make-iso identity identity))

(module+ test
  (check-equal? (iso-forward list<->vector '(1)) #(1))
  (check-equal? (iso-backward list<->vector #(1)) '(1))
  (check-equal? (iso-forward list<->vector '((1))) (vector (list 1)))
  (check-equal? (iso-forward identity-iso 1) 1)
  (check-equal? (iso-backward identity-iso 1) 1)
  (check-pred (conjoin iso? lens? prism? traversal?) list<->vector)
  (check-equal? (traversal? list<->vector) #t)
  (check-equal? (iso-modify vector<->list #(1 2 3) (lambda (l) (map add1 l))) #(2 3 4))
  (check-equal? (iso-forward list-reverse-iso '(1 2 3)) '(3 2 1))
  (check-equal? (iso-backward list-reverse-iso '(1 2 3)) '(3 2 1)))

(define iso-modify traversal-map)



; reverse an isomorphism
(define (iso-reverse iso) (make-iso (λ (a) (iso-backward iso a)) (λ (a) (iso-forward iso a))))

(module+ test
  (define vector<->list* (iso-reverse list<->vector))
  (check-equal? (iso-forward vector<->list* #(1)) '(1)))

; compose two isomorphisms
(define (iso-compose2 a<->b b<->c)
  (make-iso (λ (a) (iso-forward b<->c (iso-forward a<->b a)))
            (λ (c) (iso-backward a<->b (iso-backward b<->c c)))))

(module+ test
  (define stream<->list (make-iso stream->list
                                  (λ (l) (foldr (λ (ele s) (stream-cons ele s)) empty-stream l))))
  (define stream<->vector (iso-compose2 stream<->list list<->vector))
  (check-equal? (iso-forward stream<->vector (stream-cons 1 empty-stream)) #(1))
  (define stream-from-vector (iso-backward stream<->vector #(1)))
  (check-pred stream? stream-from-vector)
  (check-equal? (stream->list stream-from-vector) '(1)))

; compose isomorphisms
(define (iso-compose . isos)
  (foldr iso-compose2 identity-iso isos))

(module+ test
  (check-equal? (iso-forward (iso-compose stream<->list list<->vector)
                             (stream-cons 1 empty-stream))
                #(1)))



(module+ test
  ; allows you to treat a symbol like a string
  (check-equal? (iso-modify symbol<->string 'foo string-upcase) 'FOO))
