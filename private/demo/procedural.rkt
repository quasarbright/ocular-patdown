#lang racket

(module+ test (require rackunit))

;; motivating example

;; A Posn is a
(struct posn [x y] #:transparent)
;; where
;; x is a number?
;; y is a number?
;; represents a position in 2D space

;; A Rect is a
(struct rect [top-left width height] #:transparent)
;; where
;; top-left is a Posn
;; width is a number?
;; height is a number?
;; represents a rectangle

;; rectangle? number? -> rectangle?
;; move the rectangle to the right
(define (rect-move-to-right rct dx)
  ;;> struct-copy makes a new rectangle with everything the same except top-left
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
#|
obj1.x.y = 42

vs

const obj2 = {
  ...obj1,
  x: {
    ...obj1.x,
    y: 42
  }
}
|#

;; teaser
#;(define (rect-move-to-right rct dx)
    (update rct
      [(rect [top-left (posn [x])])
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
  (posn-modify-x pos (lambda (x) (+ x dx))))

;; rect? (number? -> number?) -> posn?
(define (rect-modify-x rct proc)
  ;;> you might want to go straight here, but it's better to make individual modifiers and i'll show why
  (rect-modify-top-left rct (lambda (pos) (posn-modify-x pos proc))))
#;(define (rect-move-to-right rct dx)
  (rect-modify-x rct (lambda (x) (+ x dx))))

;; A (Modifier Target Focus) is a (Target (Focus -> Focus) -> Target)
;; Represents an immutable updater.
;; The Target is the overall structure
;; The Focus is the piece of the target that gets updated
;; Examples:
;; rect-modify-top-left is a (Modifier Rect Posn)
;; posn-modify-x is a (Modifier Posn Number)
;; rect-modify-x is a (Modifier Rect Number)

;;> emphasize the language of target and focus

;; (Modifier Target Middle) (Modifier Middle Focus) -> (Modifier Target Focus)
;; The focus of the first modifier is the target of the second
(define (modifier-compose mod-t-m mod-m-f)
  ;; proc is a (Focus -> Focus)
  (lambda (target proc) (mod-t-m (lambda (middle) (mod-m-f middle proc)))))

;; (Modifier rect? number?)
#;(define rect-modify-x (modifier-compose rect-modify-top-left posn-modify-x))

;;> if you're making a library
;;> we can make struct-modify-field macro
#;(define rect-modify-top-left (struct-field-modifier rect top-left))
;; or even easier:
#;(define-struct-modifiers rect)
;; defines: rect-modify-top-left, rect-modify-width, rect-modify-height

;;> you also might want to just get the value, so in practice, we package the getter and modifier together in a lens
;;> ex: move rectangle to the right by its width

;; A (Lens Target Focus) is a
(struct lens [getter modifier] #:transparent)
;; where
;; getter is a (Target -> Focus)
;; modifier is a (Modifier Target Focus)
;;               (Target (Focus -> Focus) -> Target)

;; (Lens rect? posn?)
(define rect-top-left-lens (lens rect-top-left rect-modify-top-left))
;; (Lens posn? number?)
(define posn-x-lens (lens posn-x posn-modify-x))

;; (Lens Target Middle) (Lens Middle Focus) -> (Lens Target Focus)
;; The focus of the first lens is the target of the second
(define (lens-compose lens-t-m lens-m-f)
  (lens
   (lambda (target)
     (let ([middle (lens-get lens-t-m target)])
       (lens-get lens-m-f middle)))
   ;; same as reversed function composition
   #;(compose (lens-getter lens-b-c) (lens-getter lens-a-b))
   ;; proc is a (Focus -> Focus)
   (lambda (target proc)
     (lens-modify lens-t-m target
                  (lambda (middle)
                    (lens-modify lens-m-f middle proc))))))

;; (Lens Target Focus) Target -> Focus
(define (lens-get lns target)
  ((lens-getter lns) target))

;; (Lens Target Focus) Target (Focus -> Focus) -> Target
(define (lens-modify lns target proc)
  ((lens-modifier lns) target proc))

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
;; A lens that targets anything and focuses on the entire target
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

;;> ok, we've talked about fields and hash keys, what about list elements?

;;; traversals

;; (Listof rect?) number? -> (Listof rect?)
(define (move-rectangles-to-right rcts dx)
  (map (lambda (rct) (rect-move-to-right rct dx)) rcts))

;; (Listof rect?) number? -> (Listof rect?)
(define (move-rectangles-to-left rcts dx)
  (map (lambda (rct) (rect-move-to-left rct dx)) rcts))

;;> looks like modify
;;> lenses? no, there's more than one focus

;; (Lens (Listof rect?) rect?)
;; modify : (Listof rect?) (rect? -> rect?) -> (Listof rect?)
;; get : (Listof rect?) -> rect?

;;> no getter, but you can fold

;; A (Traversal Target Focus) is a
(struct traversal [folder modifier] #:transparent)
;; where
;; folder is a {X} (Target (Focus X -> X) X -> X) ; note the argument order is different from foldl
;; modifier is a (Modifier Target Focus) = (Target (Focus -> Focus) -> Target)
;; note the argument order is different from map
;; Represents an optic that focuses on zero or more parts of the target

;; {X} (Traversal (Listof X) X)
(define list-traversal (traversal (lambda (lst proc init) (foldl proc init lst))
                                  (lambda (lst proc) (map proc lst))))

;;> we do foldl because structures like streams don't support foldr nicely

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
  (check-equal? (traversal-modify lol-traversal '((5) () (4 3 2) (1) ()) add1)
                '((6) () (5 4 3) (2) ()))
  (check-equal? (traversal-fold lol-traversal '((5) () (4 3 2) (1) ()) cons '())
                '(1 2 3 4 5)))

;; (Traversal Target Middle) (Traversal Middle Focus) -> (Traversal Target Focus)
;; The foci of the first traversal are used as the targets for the second
(define (traversal-compose trv-t-m trv-m-f)
  (traversal
   ;; proc is a (Focus X -> X)
   ;; init is a X
   (lambda (proc init target)
     (traversal-fold trv-t-m target
                     (lambda (middle x)
                       (traversal-fold trv-m-f middle proc x))
                     init))
   ;; proc is a (Focus -> Focus)
   (lambda (target proc)
     (traversal-modify trv-t-m target
                       (lambda (middle)
                         (traversal-modify trv-m-f middle proc))))))

(define (traversal-modify trv target proc) ((traversal-modifier trv) target proc))
(define (traversal-fold trv target proc init) ((traversal-folder trv) target proc init))

(define (traversal-compose* . trvs)
  (foldr traversal-compose
         identity-traversal
         trvs))

;; {A} (Traversal A A)
;; A traversal that targets and value and only has one focus: the target itself
(define identity-traversal
  (traversal
   (lambda (target proc init) (proc target init))
   (lambda (target proc) (proc target))))

;;> what is the relationship between lenses and traversals?

;; {Target Focus} (Lens Target Focus) -> (Traversal Target Focus)
(define (lens->traversal lns)
  (traversal
   (lambda (target proc init) (proc (lens-get lns target) init))
   (lambda (target proc) (lens-modify lns target proc))))

;;> note: (lens->traversal identity-lens) = identity-traversal

;;> traversals are most useful when composed with lenses and other optics

#;(define (move-rectangles-to-right rcts dx)
    (traversal-modify (traversal-compose* list-traversal
                                          ;;> the library does this conversion automatically
                                          (lens->traversal rect-top-left-lens)
                                          (lens->traversal posn-x-lens))
                      rcts
                      (lambda (x) (+ x dx))))

;;> so far, we've been looking at data inside of a target and updating it.
;;> There is a related abstraction for when you have two different but equivalent representations of the same data.

;;; isomorphisms

;; Symbol -> Symbol
(define (symbol-upcase sym)
  (string->symbol (string-upcase (symbol->string sym))))

(module+ test
  (check-equal? (symbol-upcase 'foo)
                'FOO))

;; A Bounds is a
(struct bounds [top-left bot-right] #:transparent)
;; where
;; top-left is a Posn
;; bot-right is a Posn

;; Example
(define bounds1 (bounds (posn 10 10) (posn 15 11))); increasing y is down
(define rect1   (rect   (posn 10 10) 5 1))

(define (bounds->rect bnd)
  (match bnd
    [(bounds (posn x1 y1) (posn x2 y2))
     (rect (posn x1 y1) (- x2 x1) (- y2 y1))]))

(define (rect->bounds rct)
  (match rct
    [(rect (posn x y) width height)
     (bounds (posn x y) (posn (+ x width) (+ y height)))]))

;; Bounds Number -> Bounds
(define (bounds-move-to-right bnd dx)
  (rect->bounds (rect-move-to-right (bounds->rect bnd) dx)))

(module+ test
  (check-equal? (bounds-move-to-right (bounds (posn 0 0) (posn 1 1)) 1)
                                      (bounds (posn 1 0) (posn 2 1))))

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
  (lens (lambda (target) (iso-forward i target))
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

;;; update

;;> I wanted something like match, but for performing immutable updates like modify

#;
(define (rect-move-to-right rct dx)
  (update rct
    [(rect [top-left (posn [x])])
     (set! x (+ (get x) dx))]))

;;> patterns look like match patterns, but they're actually building optics and then set! does a modify!

#;
(match lst
  [(cons a (cons b c)) body])

#;
(if (cons? lst)
    (let ([a (car lst)])
      (let ([tmp (cdr lst)])
        (if (cons? tmp)
            (let ([b (car tmp)])
              (let ([c (cdr tmp)])
                body))
            (error "fail"))))
    (error "fail"))

#;
(update lst
  [(cons a (cons b c)) body])
#;
(let ([a car-lens])
  (let ([tmp cdr-lens])
    (let ([b (lens-compose tmp car-lens)])
      (let ([c (lens-compose tmp cdr-lens)])
        body))))
;;> we don't even mention lst
;;> no type checks or on-fail
;;> nested patterns leads to calling accessors on fields in match,
;;> but leads to composing optics in update

;;> need some special book-keeping and reference compilers for set! to work how we want
;;> parameter for current target value. set! does an immutable update and then mutates the parameter

#|
pat := _
     | id
     | (cons car-pat cdr-pat)
     | (list pat ...)
     | (list-of pat)                   ; list traversal
     | (struct-id field-spec ...)      ; struct field lens(es)
     | (iso a->b-expr b->a-expr b-pat) ; isomorphism
     | (optic optic-expr focus-pat)    ; arbitrary optic pattern
     | (and pat ...+)                  ; multiple patterns on the same value

field-spec := [field-id field-pat]
            | field-id             ; same as [field-id field-id]
|#

;;> you actually only need optic + and
;;> the syntax spec/compiler only has variables, optic, and and.
;;> we use a pattern macro extension class and implement pattern macros to define stuff like cons

#;
(update lst
  [(cons a (cons b c)) body])
#;
(update lst
  [(and (optic car-lens a)
        (optic cdr-lens (and (optic car-lens b)
                             (optic cdr-lens c))))])

#;(define-update-syntax cons (syntax-rules () [(cons a d) (and (optic car-lens a) (optic cdr-lens d))]))

;; big example
#;
(define (move-lob-to-right lob dx)
  (update lob
    [(listof (iso bounds->rect rect->bounds
                  (rect [top-left (posn [x])])))
     (modify! x (lambda (x) (+ x dx)))]))
#;
(define (move-lob-to-right lob dx)
  (update lob
    [(optic list-traversal
            (optic (iso bounds->rect rect->bounds)
                   (optic (struct-lens rect top-left)
                          (optic (struct-lens posn x)
                                 x))))
     (modify! x (lambda (x) (+ x dx)))]))

;;> if you try to implement listof patterns in match, it ends up being really hard. but here, it's trivial.
