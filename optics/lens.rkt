#lang racket

#|
A lens is a first-class getter and setter for a part of a structure.
For example:

>(lens-get car-lens (cons 1 2))
1
>(lens-set car-lens (cons 1 2) 3)
(cons 3 2)

In the example of car-lens, a cons pair is the lens' target, and its car is the lens' focus.
The setter doesn't mutate the target. It returns a copy with the focus changed.
A lens can have a focus deep within its target. Lenses can be composed, or "chained" to create
lenses which operate deeply within a structure.
For example, (lens-compose circle-center-lens posn-x-lens) focuses on a circle's x-coordinate.
|#

(module+ test (require rackunit))

(provide
 ; generic interface for lens
 gen:lens
 #;(-> any/c boolean?)
 ; lens predicate
 lens?
 #;(make-lens getter setter)
 #;(-> (-> target/c focus/c) (-> target/c focus/c target/c) lens?)
 ; (getter target) retrieves the focus from the target
 ; (setter target new-focus) returns a copy of the target with the old focus replaced by the new one
 make-lens
 #;(-> lens? target/c focus/c)
 ; get the focus
 lens-get
 #;(-> lens? target/c focus/c target/c)
 ; set the focus
 lens-set
 #;(-> lens? target/c (-> focus/c focus/c) target/c)
 ; apply a function to the focus
 ; alias for traversal-map
 lens-modify
 #;(-> lens? ... lens?)
 ; compose lenses where the first lens is shallow and the last is deep
 ; for example, (lens-compose circle-center-lens posn-x-lens) focuses on
 ; the x-coordinate of the center of a circle
 lens-compose
 #;lens?
 ; focus is the same as the target. setting the focus replaces the target.
 ; identity of lens composition
 identity-lens
 #;lens?
 ; focuses on the car of a cons pair
 car-lens
 #;lens?
 cdr-lens
 #;lens?
 caar-lens
 #;lens?
 cadr-lens
 #;lens?
 cdar-lens
 #;lens?
 cddr-lens
 #;(struct-lens struct-name:struct-id field-name:id)
 #;(struct-lens struct-name:struct-id field-name:id #:parent parent-struct-name:struct-id)
 ; creates a lens that focuses on a field of a struct.
 ; for fields that are part of a super type, supply #:parent.
 ; NOTE: if you use a super-type's lens to update a subtype, you will get an instance of the super-type, not the sub-type.
 struct-lens)



(require (for-syntax syntax/parse syntax/parse/class/struct-id)
         racket/generic
         "./traversal.rkt")



(define-generics lens
  (lens-get lens target)
  (lens-set lens target focus))

(struct make-lens (getter setter)
  #:methods gen:lens
  [(define (lens-get lens target) ((make-lens-getter lens) target))
   (define (lens-set lens target focus)
     ((make-lens-setter lens) target focus))]
  #:methods gen:traversal
  [(define (traversal-map lens target proc)
     (lens-set lens target (proc (lens-get lens target))))
   (define (traversal-foldl lens target proc init)
     (proc (lens-get lens target) init))])
; (getter target) retrieves the focus from the target
#;(-> target/c focus/c)
; (setter target new-focus) sets the focus in the target
#;(-> target/c focus/c target/c)

#; lens?
; lens targeting a cons and focusing on the car
(define car-lens (make-lens car (λ (pair a) (cons a (cdr pair)))))
#; lens?
; lens targeting a cons and focusing on the cdr
(define cdr-lens (make-lens cdr (λ (pair d) (cons (car pair) d))))
#; lens?
; identity of lens composition. get returns the target, set replaces the target
(define identity-lens (make-lens identity (λ (target focus) focus)))



#; (-> lens? any/c (-> any/c any/c) any/c)
; applies func to focus and updates target
; alias for traversal-map
(define lens-modify traversal-map)

(module+ test
  (check-equal? (lens-get car-lens (cons 1 2)) 1)
  (check-equal? (lens-get cdr-lens (cons 1 2)) 2)
  (check-equal? (lens-set car-lens (cons 1 2) 3) (cons 3 2))
  (check-equal? (lens-set cdr-lens (cons 1 2) 3) (cons 1 3))
  (check-equal? (lens-modify car-lens (cons 1 2) -) (cons -1 2))
  (check-equal? (traversal? car-lens) #t))

#; (-> lens? lens? lens?)
; composes two lenses. outer-lens' focus should be inner-lens' target.
; for example, (lens-compose2 circle-center-lens posn-x-lens)
(define (lens-compose2 outer-lens inner-lens)
  (make-lens (λ (outer-target)
               (lens-get inner-lens (lens-get outer-lens outer-target)))
             (λ (outer-target inner-focus)
               (define outer-focus (lens-get outer-lens outer-target))
               (define inner-target outer-focus)
               (define new-inner-target (lens-set inner-lens inner-target inner-focus))
               (define new-outer-focus new-inner-target)
               (lens-set outer-lens outer-target new-outer-focus))))

(define caar-lens (lens-compose2 car-lens car-lens))
; it's flipped. function composition and lens composition are in opposite orders
(define cadr-lens (lens-compose2 cdr-lens car-lens))
(define cdar-lens (lens-compose2 car-lens cdr-lens))
(define cddr-lens (lens-compose2 cdr-lens cdr-lens))

(module+ test
  (define pair4 (cons (cons 1 2) (cons 3 4)))
  (check-equal? (lens-get caar-lens pair4) 1)
  (check-equal? (lens-set caar-lens pair4 'a) (cons (cons 'a 2) (cons 3 4)))
  (check-equal? (lens-modify caar-lens pair4 -) (cons (cons -1 2) (cons 3 4)))
  (check-equal? (lens-get cadr-lens pair4) (cadr pair4))
  (check-equal? (lens-set cadr-lens pair4 'a) (cons (cons 1 2) (cons 'a 4)))
  (check-equal? (lens-modify cadr-lens pair4 -) (cons (cons 1 2) (cons -3 4))))

#; (-> lens? ... lens?)
; composes lenses, "outer" lenses first
(define (lens-compose . lenses)
  (foldr lens-compose2 identity-lens lenses))

(module+ test
  (check-equal? (lens-get (lens-compose cdr-lens car-lens) pair4) (cadr pair4)))

; creates a lens that focuses on a field of a struct.
; for fields that are part of a super type, supply #:parent.
; NOTE: if you use a super-type's lens to update a subtype, you will get an instance of the super-type, not the sub-type.
(define-syntax struct-lens
  (syntax-parser
    [(_ struct-name:struct-id field-name:id (~optional (~seq #:parent parent-struct-name:struct-id)))
     #'(make-lens
        (λ (target) (match target [(struct* (~? parent-struct-name struct-name) ([field-name focus])) focus]))
        (λ (target new-focus) (struct-copy struct-name target [field-name (~? (~@ #:parent parent-struct-name) (~@)) new-focus])))]))

(module+ test
  (struct posn [x y] #:transparent)
  (define posn-x-lens (struct-lens posn x))
  (define posn34 (posn 3 4))
  (check-equal? (lens-get posn-x-lens posn34) 3)
  (check-equal? (lens-set posn-x-lens posn34 5) (posn 5 4))
  (check-equal? (lens-modify posn-x-lens posn34 -) (posn -3 4)))

(module+ test
  (struct posn3 posn [z] #:transparent)
  (define posn3-z-lens (struct-lens posn3 z))
  (define posn3-x-lens (struct-lens posn3 x #:parent posn))
  (define posn345 (posn3 3 4 5))
  (check-equal? (lens-get posn3-z-lens posn345) 5)
  (check-equal? (lens-set posn3-z-lens posn345 6) (posn3 3 4 6))
  (check-equal? (lens-modify posn3-z-lens posn345 -) (posn3 3 4 -5))
  (check-equal? (lens-get posn-x-lens posn345) 3)
  ; using a super-type's lens to update an instance of subtype results in an instance of the supertype
  (check-equal? (lens-set posn-x-lens posn345 1) (posn 1 4))
  (check-equal? (lens-modify posn-x-lens posn345 -) (posn -3 4))
  (check-equal? (lens-get posn3-x-lens posn345) 3)
  (check-equal? (lens-set posn3-x-lens posn345 1) (posn3 1 4 5))
  (check-equal? (lens-modify posn3-x-lens posn345 -) (posn3 -3 4 5)))
