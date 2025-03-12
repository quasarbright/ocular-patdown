#lang racket

(module+ test (require rackunit))

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
   ;; same as reversed function composition
   #;(compose (lens-getter lens-b-c) (lens-getter lens-a-b))
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

;; Lens ... -> Lens
;; variadic lens composition
(define (lens-compose* . ls)
  (foldr lens-compose
         identity-lens
         ls))

;;> ask students what should be the base case for lens-compose?
;;> what is 0 lenses composed together?
;;> what is the identity of lens composition?

;; {A} (Lens A A)
(define identity-lens
  (lens (lambda (target) target)
        (lambda (target proc) (proc target))))

;; K -> (Lens (Hash K V) V)
(define (hash-key-lens key)
  (lens (lambda (h) (hash-ref h key))
        (lambda (h proc) (hash-set h key (proc (hash-ref h key))))))

(define player
  (hash 'head (rect (posn 0 5) 1 1)
        'chest (rect (posn -1 4) 2 4)
        'hand (rect (posn 1 3) 1 1)))

;; wave hello by moving the hand to the right
(define player/wave
  (lens-modify (lens-compose* (hash-key-lens 'hand)
                              rect-top-left-lens
                              posn-x-lens)
               player
               add1))

(module+ test
  (check-equal? (hash-ref player/wave 'hand)
                (rect (posn 2 3) 1 1)))

;;> this is overkill, but it becomes necessary when you have very deep immutable updates

;;; traversals

;; (Listof rect?) number? -> (Listof rect?)
(define (move-rectangles-to-right rcts dx)
  (map (lambda (rct) (rect-move-to-right rct dx)) rcts))

;; (Listof rect?) number? -> (Listof rect?)
(define (move-rectangles-to-left rcts dx)
  (map (lambda (rct) (rect-move-to-left rct dx)) rcts))

;;> no getter, but you can fold

;; A (Traversal A B) is a
(struct traversal [folder modifier] #:transparent)
;; where
;; folder is a {X} (A (B X -> X) X -> X) ; note the argument order is different from foldl
;; modifier is a (Modifier A B) = (A (B -> B) -> A) ; note the argument order is different from map
;; A is the type of the target
;; B is the type of the foci
;; Represents an optic that focuses on zero or more parts of the target

;; {X} (Traversal (Listof X) X)
(define list-traversal (traversal (lambda (lst proc init) (foldl proc init lst))
                                  (lambda (lst proc) (map proc lst))))

;; {X} (Traversal (Listof (Listof X)) X)
(define lol-traversal
  (traversal
   (lambda (lol proc init)
     ;;> this is tricky. start with the outer fold, realize acc in the inner lambda
     ;;> is the result from the previous inner list and use it as the init for the next list
     (foldl (lambda (lox acc) (foldl proc acc lox))
            init
            lol))
   (lambda (lol proc)
     (map (lambda (lox)
            (map proc lox))
          lol))))

(module+ test
  (check-equal? (traversal-fold lol-traversal '((5) () (4 3 2) (1) ()) cons '())
                '(1 2 3 4 5)))

;; (Traversal A B) (Traversal B C) -> (Traversal A C)
(define (traversal-compose trv-a-b trv-b-c)
  (traversal
   ;; proc is a (C X -> X)
   ;; init is a X
   (lambda (proc init a)
     (traversal-fold trv-a-b
                     a
                     (lambda (b x)
                       (traversal-fold trv-b-c b proc x))
                     init))
   ;; proc is a (C -> C)
   (lambda (a proc)
     (traversal-modify trv-a-b a (lambda (b) (traversal-modify trv-b-c b proc))))))

(define (traversal-modify trv target proc) ((traversal-modifier trv) target proc))
(define (traversal-fold trv target proc init) ((traversal-folder trv) target proc init))

(define (traversal-compose* . trvs)
  (foldr traversal-compose
         identity-traversal
         trvs))

;; {A} (Traversal A A)
(define identity-traversal
  (traversal
   (lambda (target proc init) (proc target init))
   (lambda (target proc) (proc target))))

;; {A B} (Lens A B) -> (Traversal A B)
(define (lens->traversal lns)
  (traversal
   (lambda (a proc init) (proc (lens-get lns a) init))
   (lambda (a proc) (lens-modify lns a proc))))

;;> note: (lens->traversal identity-lens) = identity-traversal

;;> traversals are most useful when composed with lenses and other optics

#;(define (move-rectangles-to-right rcts dx)
    (traversal-modify (traversal-compose* list-traversal
                                          ;;> the library does this conversion automatically
                                          (lens->traversal rect-top-left-lens)
                                          (lens->traversal posn-x-lens))
                      rcts
                      (lambda (x) (+ x dx))))

;;; isomorphisms

;; Symbol -> Symbol
(define (symbol-upcase sym)
  (string->symbol (string-upcase (symbol->string sym))))

(struct bounds [top-left bot-right] #:transparent)

;; Bounds Number -> Bounds
(define (bounds-move-to-right bnd dx)
  (rect->bounds (rect-move-to-right (bounds->rect bnd) dx)))

(module+ test
  (check-equal? (bounds-move-to-right (bounds (posn 0 0) (posn 1 1)) 1)
                (bounds (posn 1 0) (posn 2 1))))

(define (bounds->rect bnd)
  (match bnd
    [(bounds (posn x1 y1) (posn x2 y2))
     (rect (posn x1 y1) (- x2 x1) (- y2 y1))]))

(define (rect->bounds rct)
  (match rct
    [(rect (posn x y) width height)
     (bounds (posn x y) (posn (+ x width) (+ y height)))]))

;;> common pattern is you have two types that are equivalent, but one is more convenient for the operation you're performing,
;;> so you convert back and forth

;; An (Iso A B) is a
(struct iso [fwd bckwd] #:transparent)
;; where
;; fwd is a (A -> B)
;; bckwd is a (B -> A)
;; CONSTRAINT: forward and backward should be inverses of each other

(define symbol<->string (iso symbol->string string->symbol))
(define bounds<->rect (iso bounds->rect rect->bounds))

;; (Iso A B) A (B -> B) -> A
(define (iso-modify i a proc)
  (iso-backward i (proc (iso-forward i a))))

(module+ test
  (check-equal? (iso-modify symbol<->string 'foo string-upcase)
                'FOO))

(define (iso-forward i a) ((iso-fwd i) a))
(define (iso-backward i b) ((iso-bckwd i) b))

;;> Isomorphism composition is pretty straightforward, but you won't need it often

;; (Iso A B) (Iso B C) -> (Iso A C)
(define (iso-compose* . is)
  (foldr iso-compose
         identity-iso
         is))

(define (iso-compose a<->b b<->c)
  (iso (lambda (a) (iso-forward b<->c (iso-forward a<->b a)))
       (lambda (c) (iso-backward a<->b (iso-backward b<->c c)))))

;; very identity!
;; {A} (Iso A A)
(define identity-iso (iso identity identity))

;;> where do isos fit into the optic hierarchy?

(define (iso->lens i)
  (lens (lambda (a) (iso-forward i a))
        (lambda (target proc) (iso-modify i target proc))))

;; (Listof Bounds) Number -> (Listof Bounds)
(define (move-lob-to-right lob dx)
  (traversal-modify (traversal-compose* list-traversal
                                        (lens->traversal (iso->lens bounds<->rect))
                                        (lens->traversal rect-top-left-lens)
                                        (lens->traversal posn-x-lens))
                    lob
                    (lambda (x) (+ x dx))))

(module+ test
  (check-equal? (move-lob-to-right (list (bounds (posn 0 0) (posn 1 1))
                                         (bounds (posn 2 2) (posn 3 3)))
                                   1)
                (list (bounds (posn 1 0) (posn 2 1))
                      (bounds (posn 3 2) (posn 4 3)))))
