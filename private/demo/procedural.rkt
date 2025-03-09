#lang racket

(module+ test (require rackunit))
(require "../../optics/traversal.rkt"
         "../../optics/isomorphism.rkt"
         "../../optics/lens.rkt"
         "../../optics/optic.rkt"
         (for-syntax syntax/parse syntax/transformer syntax/parse/class/struct-id))

;; motivating example

(struct posn [x y] #:transparent)
(struct rect [top-left width height] #:transparent)

;; rectangle? number? -> rectangle?
;; move the rectangle to the right
(define (rect-move-to-right rct dx)
  (struct-copy rect rct [top-left (posn-move-to-right (rect-top-left rct) dx)]))

(module+ test
  (check-equal? (rect-move-to-right (rect (posn 0 0) 10 30) 2)
                (rect (posn 2 0) 10 30)))

;; posn? number? -> rectangle?
;; move the posn to the right
(define (posn-move-to-right pos dx)
  (struct-copy posn pos [x (+ (posn-x pos) dx)]))

(module+ test
  (check-equal? (posn-move-to-right (posn 1 2) 4)
                (posn 5 2)))

;;> so far we haven't gone deep. at work, I occasionally need deep immutable updates that go down several levels
;;> maybe even show the js example of modifying a field deep in a json

;; teaser
#;(define (rect-move-to-right rct dx)
    (update rct
      [#;(struct-field rect top-left (struct-lens posn x))
       (rect [top-left (posn [x])])
       (set! x (+ (get x) dx))]))

;;; lenses

;; rectangle? number? -> rectangle?
;; move the rectangle to the left
(define (rect-move-to-left rct dx)
  (struct-copy rect rct [top-left (posn-move-to-left (rect-top-left rct) dx)]))

(module+ test
  (check-equal? (rect-move-to-left (rect (posn 10 0) 10 30) 2)
                (rect (posn 8 0) 10 30)))

;; posn? number? -> rectangle?
;; move the posn to the left
(define (posn-move-to-left pos dx)
  (struct-copy posn pos [x (- (posn-x pos) dx)]))

(module+ test
  (check-equal? (posn-move-to-left (posn 10 2) 4)
                (posn 6 2)))

;;> same thing except the modifier is different

;; rectangle? (posn? -> posn?) -> rectangle?
(define (rect-modify-top-left rct proc)
  (struct-copy rect rct [top-left (proc (rect-top-left rct))]))

;; posn? (number? -> number?) -> posn?
(define (posn-modify-x pos proc)
  (struct-copy posn pos [x (proc (posn-x pos))]))

;; rectangle? number? -> rectangle?
;; move the rectangle to the right
#;(define (rect-move-to-right rct dx)
  (rect-modify-top-left rct (lambda (pos) (posn-move-to-right pos dx))))

;; posn? number? -> rectangle?
;; move the posn to the right
#;(define (posn-move-to-right pos dx)
  (posnt-modify-x pos (lambda (x) (+ x dx))))

;; rect? (number? -> number?) -> posn?
(define (rect-modify-x rct proc)
  (rect-modify-top-left rct (lambda (pos) (posn-modify-x pos proc))))
#;(define (rect-move-to-right rct dx)
  (rect-modify-x rct (lambda (x) (+ x dx))))

;; A (Modifier A B) is a (A (B -> B) -> A)
;; Represents an immutable updater.
;; A is the "target" (the overall structure)
;; B is the "focus" (the piece of the target that gets updated)
;; Ex: rect-modify-top-left, posn-modify-x

;; (Modifier A B) (Modifier B C) -> (Modifier B C)
(define (modifier-compose mod-a-b mod-b-c)
  ;; proc is a (c -> c)
  (lambda (a proc) (mod-a-b a (lambda (b) (mod-b-c b proc)))))

;; (Modifier rect? number?)
#;(define rect-modify-x (modifier-compose rect-modify-top-left posn-modify-x))

;;> we can make struct-modify-field macro, show what it would look like to use

;;> you also might want to just get the value, so in practice, we package the getter and modifier together in a lens
;;> ex: move rectangle to the right by its width

;; A (Lens A B) is a
(struct lens [getter modifier] #:transparent)
;; where
;; getter is a (A -> B)
;; modifier is a (Modifier A B)
;;               (A (B -> B) -> A)

;; (Lens rect? posn?)
(define rect-top-left-lens (lens rect-top-left rect-modify-top-left))
;; (Lens posn? number?)
(define posn-x-lens (lens posn-x posn-modify-x))

;; (Lens A B) (Lens B C) -> (Lens A C)
(define (lens-compose lens-a-b lens-b-c)
  (lens
   (lambda (a)
     (let ([b (lens-get lens-a-b a)])
       (lens-get lens-b-c b)))
   ;; proc is a (c -> c)
   (lambda (a proc)
     (lens-modify lens-a-b a
                  (lambda (b)
                    (lens-modify lens-b-c b proc))))))

;; (Lens A B) A -> B
(define (lens-get lns a)
  ((lens-getter lns) a))

;; (Lens A B) A (B -> B) -> A
(define (lens-modify lns a proc)
  ((lens-modifier lns) a proc))

;; K -> (Lens (Hash K V) V)
(define (hash-key-lens key)
  (lens (lambda (h) (hash-ref h key))
        (lambda (h proc) (hash-set h key (proc (hash-ref h key))))))

#;(define player
    (hash 'head (rect ...)
          'chest (rect ...)))

;;> this is overkill, but it becomes necessary when you have very deep immutable updates
