#lang slideshow

(require slideshow/code slideshow/text pict/color)

(define (vert . picts) (apply vl-append 10 picts))
(define (horiz . picts) (apply hc-append 5 picts))

(define-syntax-rule (target datum ...)
  (highlight (code datum ...) #:color (light (light "blue"))))

(define-syntax-rule (focus datum ...)
  (highlight (code datum ...) #:color "orange" #:padding 0))

(define (highlight p #:color [color "yellow"] #:padding [padding 10])
  (cc-superimpose
   (inset (filled-rectangle (+ padding (pict-width p)) (+ padding (pict-height p))
                            #:color color
                            #:draw-border? #f)
          (- (+ padding 10)))
   p))

(define-syntax-rule (hide-code datum ...)
  (hide (code datum ...)))

(define (hide p)
  (cellophane p 0.1))

(slide
 (titlet "A Match-Like DSL for Deep Immutable Updates")
 (titlet "Mike Delmonaco"))

(begin
  ; needed to do multiple slides instead of 'next bc of centering issues with multiple uses of code
  (slide
   #:title "A Sneak Peek"
   #:layout 'top
   (code
    (define lst (list 1 2 3 4))))

  (slide
   #:title "A Sneak Peek"
   #:layout 'top
   (code
    (define lst (list 1 2 3 4))
    code:blank
    > (update lst
        [(list a b c d)
         (set! c 99)])
    (list 1 2 99 4)))

  (slide
   #:title "A Sneak Peek"
   #:layout 'top
   (code
    (define lst (list 1 2 3 4))
    code:blank
    > (update lst
        [(list a b c d)
         (set! c 99)])
    (list 1 2 99 4)
    code:blank
    > lst
    (list 1 2 3 4)
    code:blank
    (code:comment "did not mutate lst!"))))

(slide
 #:title "Agenda"
 (item "Discovering Lenses")
 (item "update DSL Overview")
 (item "update DSL Implementation"))

(slide
 #:title "Moving Rectangles"
 (t "Some data types we'll use in our examples")
 (code
  (struct posn [x y] #:transparent)
  (struct rect [top-left width height] #:transparent)
  (define red-rectangle (rect (posn 100 50) 100 200)))
 (scale (pin-over
         (rectangle 300 300)
         100 50
         (filled-rectangle 100 200 #:color "red"))
        0.75))

(slide
 #:title "Moving Rectangles"
 (t "To update a field, you use struct-copy")
 (code
  (code:comment "rect? number? -> rect?")
  (define (rect-move-to-right rct dx)
    (struct-copy
     rect rct
     [top-left (posn-move-to-right (rect-top-left rct)
                                   dx)]))
  (rect-move-to-right red-rectangle 50))
 (scale (hc-append
         (pin-over
          (rectangle 300 300)
          100 50
          (filled-rectangle 100 200 #:color "red"))
         (arrow 30 0)
         (pin-over
          (rectangle 300 300)
          150 50
          (filled-rectangle 100 200 #:color "red")))
        0.75))

(slide
 #:title "Moving Rectangles"
 (t "What about to the left?")
 (code
  (code:comment "rect? number? -> rect?")
  (define (rect-move-to-left rct dx)
    (struct-copy
     rect rct
     [top-left (posn-move-to-left (rect-top-left rct)
                                  dx)]))
  (rect-move-to-left red-rectangle 50))
 (scale (hc-append
         (pin-over
          (rectangle 300 300)
          100 50
          (filled-rectangle 100 200 #:color "red"))
         (arrow 30 0)
         (pin-over
          (rectangle 300 300)
          50 50
          (filled-rectangle 100 200 #:color "red")))
        0.75))

(define-syntax-rule (framed-code datum ...)
  (frame (code datum ...)
         #:color "red"
         #:line-width 3))

(slide
 #:title "Spot the Difference"
 (code
  (define (rect-move-to-right rct dx)
    (struct-copy
     rect rct
     [top-left (#,(framed-code posn-move-to-right) (rect-top-left rct)
                                   dx)]))
  code:blank
  (define (rect-move-to-left rct dx)
    (struct-copy
     rect rct
     [top-left (#,(framed-code posn-move-to-left) (rect-top-left rct)
                                  dx)])))
 (t "The difference is what you do to the top-left"))

(slide
 #:title "Modifiers"
 (para "We can make an abstraction for doing something to the top-left")
 (code
  (code:comment "rect? (posn? -> posn?) -> rect?")
  (define (rect-modify-top-left rct proc)
    (struct-copy
     rect rct
     [top-left (#,(framed-code proc) (rect-top-left rct))]))))

(slide
 #:title "Modifiers"
 (t "Similar for a posn's x-value")
 (code
  (code:comment "posn? (number? -> number?) -> posn?")
  (define (posn-modify-x p proc)
    (struct-copy
     posn p
     [x (#,(framed-code proc) (posn-x p))]))))

(slide
 #:title "Modifiers"
 (horiz (t "The") (highlight (t "Target") #:color (light (light "blue"))) (t "is the overall structure"))
 (horiz (t "The") (highlight (t "Focus") #:color "orange") (t "is the piece we're modifying"))
 (code
  rect-modify-top-left
  #,(target (rect #,(focus (posn x y)) width height))
  code:blank
  posn-modify-x
  #,(target (posn #,(focus x) y))
  code:blank))

(slide
 #:title "Modifiers"
 (vert
  (horiz (t "A") (code (Modifier Target Focus)) (t "is a"))
  (code (Target (Focus -> Focus) -> Target))
  (t "Represents an immutable updater.")
  (horiz (t "The") (code Target) (t "is the overall structure"))
  (horiz (t "The") (code Focus) (t "is the piece of the target"))
  (t "that gets modified.")
  (t "Examples:")
  (table 3
         (list (code rect-modify-top-left) (t "is a") (code (Modifier Rect Posn))
               (code posn-modify-x) (t "is a") (code (Modifier Posn Number))
               )
         lc-superimpose
         cc-superimpose
         10
         10)))

(slide
 #:title "Modifiers"
 (t "Putting it to use")
 (code
  (define (rect-move-to-right rct dx)
    (rect-modify-top-left
     rct
     (lambda (top-left)
       (posn-modify-x
        top-left
        (lambda (x) (+ x dx)))))))
 (t "Doesn't look much better :(")
 'next
 (t "This is a modify in a modify. Can we make rect-modify-x?"))

(slide
 #:title "Modifiers"
 (code
  (code:comment "(Modifier rect? posn?)")
  rect-modify-top-left
  #,(target (rect #,(focus (posn x y)) width height))
  code:blank
  (code:comment "(Modifier posn? number?)")
  posn-modify-x
  #,(target (posn #,(focus x) y))
  code:blank
  (code:comment "(Modifier rect? number?)")
  (define rect-modify-x
    (modifier-compose rect-modify-top-left
                      posn-modify-x))
  #,(target (rect (posn #,(focus x) y) width height))))

(slide
 #:title "Modifiers"
 (code
  (code:comment "(Modifier A B) (Modifier B C) -> (Modifier A C)")
  (define (modifier-compose a-modify-b b-modify-c)
    (code:comment "proc is a (C -> C)")
    (lambda (a proc)
      (a-modify-b
       a
       (lambda (b)
         (b-modify-c
          b
          proc)))))))

(slide
 #:title "Modifiers"
 (t "Putting it to use")
 (code
  (define (rect-move-to-right rct dx)
    (rect-modify-x rct
      (lambda (x) (+ x dx))))))

(slide
 #:title "Lenses"
 (t "Package getter and modifier together")
 (code
  (code:comment "(Lens rect? posn?)")
  (define rect-top-left-lens
    (lens rect-top-left
          rect-modify-top-left))
  #,(target (rect #,(focus (posn x y)) width height)))
 (horiz (t "Like a first-class, immutable ") (code .top-left)))

(slide
 #:title "Lenses"
 (vert
  (horiz (t "A") (code (Lens Target Focus)) (t "is a"))
  (code (struct lens [getter modifier] #:transparent))
  (t "where")
  (horiz (code getter) (t "is a") (code (Target -> Focus)))
  (horiz (code modifier) (t "is a") (code (Target (Focus -> Focus) -> Target)))
  (t " ")
  (t "Examples:")
  (code
   (code:comment "(Lens rect? posn?)")
   (define rect-top-left-lens
     (lens rect-top-left
           rect-modify-top-left))
   #,(target (rect #,(focus (posn x y)) width height)))))

(slide
 #:title "Lenses"
 (t "We can compose lenses like modifiers")
 (code
  (code:comment "(Lens A B) (Lens B C) -> (Lens A C)")
  (define (lens-compose lens-a-b lens-b-c) ...)
  code:blank
  rect-top-left-lens
  #,(target (rect #,(focus (posn x y)) width height))
  code:blank
  posn-x-lens
  #,(target (posn #,(focus x) y))
  code:blank
  (lens-compose rect-top-left-lens posn-x-lens)
  #,(target (rect (posn #,(focus x) y) width height))))

(slide
 #:title "Other Lenses"
 (code
  (code:comment "(Lens (Cons A B) A)")
  (define car-lens
    (lens car
          (lambda (pair proc)
            (cons (proc (car pair))
                  (cdr pair)))))
  #,(target (cons #,(focus a) b))))

(slide
 #:title "Other Lenses"
 (code
  (code:comment "K -> (Lens (Hash K V) V)")
  (define (hash-key-lens key)
    (lens (lambda (h) (hash-ref h key))
          (lambda (h proc)
            (if (hash-has-key? h key)
                (hash-set h
                          key
                          (proc (hash-ref h key)))
                h))))
  #,(target (hash ... key #,(focus value) ...))))

#;
(define full-identity-lens
  (code
   (code:comment "A -> A")
   (define identity
     (lambda (a) a))
   (code:comment "(Lens A A)")
   (define identity-lens
     (lens (lambda (a) a)
           (code:comment "A (A -> A) -> A")
           (lambda (a proc) (proc a))))
   #,(target #,(focus a))))

#;
(slide
 #:title "The Identity Lens"
 (pin-over
  (ghost full-identity-lens)
  0 0
  (code
   (code:comment "A -> A")
   (define identity
     (lambda (a) a))
   (code:comment "(Lens A A)")
   (define identity-lens
     ???))))

#;
(slide
 #:title "The Identity Lens"
 full-identity-lens)

#;
(slide
 #:title "The Identity Lens"
 (code
  (define (lens-compose* . ls)
    (foldr lens-compose
           identity-lens
           ls))
  code:blank
  (lens-compose lns identity-lens)
  (code:comment "same as")
  lns))

(slide
 #:title "Lens Laws"
 'next
 (para "Lenz's law states that the direction of the electric current induced in a conductor by a changing magnetic field is such that the magnetic field created by the induced current opposes changes in the initial magnetic field. It is named after physicist Heinrich Lenz, who formulated it in 1834."))

(slide
 #:title "Lens Laws"
 (t "You get back what you put in")
 (codeblock-pict "get(set(t,f)) = f")
 (code
  code:blank
  code:blank
  (equal? (lens-get lens-t-f
                    (lens-set lens-t-f
                              t f))
          f)))

(slide
 #:title "Lens Laws"
 (t "Putting back what you get out does nothing")
 (codeblock-pict "set(t,get(t)) = t")
 (code
  code:blank
  code:blank
  (equal? (lens-set lens-t-f
                    t (lens-get lens-t-f t))
          t)))

(slide
 #:title "Lens Laws"
 (t "Setting twice is the same as setting once")
 (codeblock-pict "set(set(t,f), f^) = set(t,f^)")
 (code
  code:blank
  code:blank
  (equal? (lens-set lens-t-f
                    (lens-set lens-t-f t f)
                    f^)
          (lens-set lens-t-f
                    t
                    f^))))

(slide
 #:title "Lenses for Lists?"
 (code
  (define (move-rectangles-to-right rcts dx)
    (map #,(framed-code
            (lambda (rct)
              (rect-move-to-right rct dx)))
         rcts)))
 'next
 (t "Looks like a modify!")
 (horiz (t "map is like ") (code list-modify-rectangles))
 (horiz (t "Can we do ") (code list-rectangles-lens) (t "?")))

(slide
 #:title "Lenses for Lists?"
 (t "Target is a list of rectangles, and we want to provide a function on rectangles")
 (code
  (code:comment "(Lens (Listof rect?) rect?)")
  list-rectangles-lens
  #,(target (list))
  #,(target (list #,(focus r1)))
  #,(target (list #,(focus r1) #,(focus r2) #,(focus r3) ...))))

(slide
 #:title "Lenses for Lists?"
 (code
  (code:comment "(Lens (Listof rect?) rect?)")
  list-rectangles-lens
  #,(framed-code (code:comment "(Listof rect?) -> rect?"))
  getter
  (code:comment "(Listof rect?) (rect? -> rect?) -> (Listof rect?)")
  modifier)
 (t "No single focus to \"get\". There are zero or more foci.")
 (code
  #,(target (list))
  #,(target (list #,(focus r1)))
  #,(target (list #,(focus r1) #,(focus r2) #,(focus r3) ...))))

(slide
 #:title "Lenses for Lists?"
 #:layout 'top
 (vert
  (horiz (t "A") (code (Lens Target Focus)) (t "is a"))
  (code (struct lens [getter modifier] #:transparent))
  (t "where")
  (frame (horiz (code getter) (t "is a") (code (Target -> Focus)))
         #:color "red"
         #:line-width 3)
  (horiz (code modifier) (t "is a") (code (Target (Focus -> Focus) -> Target)))
  (blank))
 (t "Can't get a single focus."))

(slide
 #:title "Traversals"
 #:layout 'top
 (vert
  (horiz (t "A") (code (Traversal Target Focus)) (t "is a"))
  (code (struct traversal [folder modifier] #:transparent))
  (t "where")
  (horiz (code folder) (t "is a") (code {X} (Target (Focus X -> X) X -> X)))
  (horiz (code modifier) (t "is a") (code (Target (Focus -> Focus) -> Target)))
  (blank))
 (t "Can't get a single focus.")
 (t "Fold over all foci instead."))

(slide
 #:title "Traversals"
 (code
  (code:comment "{X} (Traversal (Listof X) X)")
  (define list-traversal
    (traversal (lambda (lst proc init)
                 (foldl proc init lst))
               (lambda (lst proc)
                 (map proc lst)))))
 (code
  #,(target (list))
  #,(target (list #,(focus a)))
  #,(target (list #,(focus a) #,(focus b) #,(focus c) ...))))

(slide
 #:title "Traversals"
 (t "We can compose them too")
 (code
  (code:comment "{X} (Traversal (Listof (Listof X)) X)")
  (define lol-traversal
    (traversal-compose list-traversal
                       list-traversal)))
 (code
  #,(target (list (list #,(focus a0) #,(focus a1) #,(focus a2) ...)
                  (list #,(focus b0) #,(focus b1) #,(focus b2) ...)
                  (list #,(focus c0) #,(focus c1) #,(focus c2) ...)
                  ...))))

#;
(slide
 #:title "Identity Traversal"
 #:layout 'top
 (code
  (code:comment "{A} (Traversal A A)")
  (define identity-traversal
    (traversal
     (lambda (a proc init) (proc a init))
     (lambda (a proc) (proc a))))
  #,(target #,(focus a))
  (code:comment "Same as identity lens?")))

#;
(slide
 #:title "Identity Traversal"
 #:layout 'top
 (code
  (code:comment "{A} (Traversal A A)")
  (define identity-traversal
    (traversal
     (lambda (a proc init) (proc a init))
     (lambda (a proc) (proc a))))
  #,(target #,(focus a))
  (code:comment "Same as identity lens?")
  (define identity-lens
    (lens
     (lambda (a) a)
     (lambda (a proc) (proc a))))
  #,(target #,(focus a))))

(slide
 #:title "Lenses vs. Traversals"
 #:layout 'top
 (para "Lenses")
 (item "one focus")
 (item "operations: get, modify")
 (item "Good for focusing on a field")
 (para "Traversals")
 (item "zero or more foci")
 (item "operations: fold, modify")
 (item "Good for focusing on elements of collections")
 'next
 (para "one focus ⇒ zero or more foci")
 'next
 (para "lenses ⊂ traversals"))

#;
(slide
 #:title "Lenses vs. Traversals"
 (code
  (code:comment "(Lens A B) -> (Traversal A B)")
  (define (lens->traversal lns)
    (traversal
     (lambda (target proc init)
       (proc (lens-get lns target) init))
     (lambda (target proc)
       (lens-modify lns target proc))))
  code:blank
  (lens->traversal identity-lens)
  (code:comment "same as")
  identity-traversal))

(slide
 #:title "Combining Lenses and Traversals"
 (t "Cast lenses to traversals and compose as traversals.")
 (code
  (traversal-compose*
   list-traversal
   (lens->traversal rect-top-left-lens)
   (lens->traversal posn-x-lens))
  code:blank
  #,(target (list (rect (posn #,(focus x1) y1) width1 height1)
                  (rect (posn #,(focus x2) y2) width2 height2)
                  ...))))

(slide
 #:title "Combining Lenses and Traversals"
 (t "Cast lenses to traversals and compose as traversals.")
 (code
  (optic-compose
   list-traversal
   rect-top-left-lens
   posn-x-lens)
  code:blank
  #,(target (list (rect (posn #,(focus x1) y1) width1 height1)
                  (rect (posn #,(focus x2) y2) width2 height2)
                  ...))))

(slide
 #:title "Equivalent Data Representations"
 (code
  (define (symbol-upcase sym)
    (string->symbol
     (string-upcase
      (symbol->string sym))))
  code:blank
  > (symbol-upcase 'foo)
  'FOO)
 (t "Convert to string, do something, convert back.")
 (t "String is more convenient for this."))

(slide
 #:title "Equivalent Data Representations"
 (t "Bounding boxes, equivalent to rectangles")
 (code
  (code:comment "A Bounds is a")
  (struct bounds [top-left bot-right] #:transparent)
  (code:comment "where")
  (code:comment "top-left is a Posn")
  (code:comment "bot-right is a Posn")
  code:blank
  (define bounds1 (bounds (posn 10 10) (posn 15 11)))
  (define rect1   (rect   (posn 10 10)        5  1)))
 (t "Let's try moving them to the right."))

(slide
 #:title "Equivalent Data Representations"
 (t "Try lenses.")
 (code
  (define (bounds-move-to-right/naive bnd dx)
    (lens-modify (lens-compose bounds-top-left-lens
                               posn-x-lens)
                 bnd
                 (lambda (x) (+ x dx))))
  #,(target (bounds (posn #,(focus left) top) (posn right bot))))
 (scale (hc-append
         (pin-over
          (rectangle 300 300)
          100 50
          (filled-rectangle 100 200 #:color "red"))
         (arrow 30 0)
         (pin-over
          (rectangle 300 300)
          200 50
          (filled-rectangle 50 200 #:color "red")))
        0.75)
 (t "The left transforms independently from the right"))

(slide
 #:title "Equivalent Data Representations"
 (horiz (code rect) (t "is more convenient for this."))
 (code
  (define (bounds->rect bnd) ...)
  (define (rect->bounds rct) ...)
  code:blank
  (define (bounds-move-to-right bnd dx)
    (rect->bounds
     (rect-move-to-right
      (bounds->rect bnd) dx))))
 (t "Convert to rectangle, do something, convert back."))

(slide
 #:title "Equivalent Data Representations"
 (t "Convert to convenient representation, do something, convert back.")
 (code
  (define (bounds-move-to-right bnd dx)
    (rect->bounds
     (#,(framed-code rect-move-to-right)
      (bounds->rect bnd)
      dx)))
  code:blank
  (define (symbol-upcase sym)
    (string->symbol
     (#,(framed-code string-upcase)
      (symbol->string sym)))))
 (t "Let's make an abstraction!"))

(slide
 #:title "Isomorphisms"
 (t "Package forward and backward conversions together")
 (code
  (define symbol<->string
    (iso symbol->string string->symbol))
  #,(target 'foo) <-> #,(focus "foo")
  code:blank
  (define bounds<->rect
    (iso bounds->rect rect->bounds))
  #,(target (bounds (posn left top) (posn right bot)))
  <->
  #,(focus (rect (posn x y) width height)))
 (t "The target is the original representation")
 (t "The \"focus\" is the equivalent, more convenient represention"))

(slide
 #:title "Isomorphisms"
 (code
  (define (symbol-upcase sym)
    (iso-modify symbol<->string
                sym
                string-upcase))))

(slide
 #:title "Isomorphisms"
 (vert
  (para "An" (code (Iso A B)) "is a")
  (code (struct iso [fwd bckwd] #:transparent))
  (para "where")
  (para (code fwd)"is a" (code (A -> B)))
  (para (code bckwd) "is a" (code (B -> A)))
  (para "CONSTRAINT: forward and backward should be inverses of each other")))

(slide
 #:title "Isomorphisms vs Lenses vs Traversals"
 (para "Lenses")
 (item "one focus")
 (item "operations: get, modify")
 (para "Traversals")
 (item "zero or more foci")
 (item "operations: fold, modify")
 (para "Isomorphisms")
 (item "one \"focus\" that is a different representation of the target")
 (item "operations: forward, backward, modify")
 'next
 (para "isomorphisms ⊂ lenses ⊂ traversals"))

#;
(slide
 #:title "Isomorphisms vs Lenses vs Traversals"
 (code
  (define (iso->lens i)
    (code:comment "get is just forward!")
    (lens (lambda (target) (iso-forward i target))
          (lambda (target proc)
            (iso-modify i target proc))))))

(slide
 #:title "Combining Isomorphisms and Other Optics"
 (t "Optic to move a bounds to the right like a rect.")
 (code
  (optic-compose bounds<->rect
                 rect-top-left-lens
                 posn-x-lens)
  code:blank
  #,(target (bounds (posn left top) (posn right bot)))
  <->
  (rect (posn #,(focus x) y) width height)))

(slide
 #:title "Combining Isomorphisms and Other Optics"
 (t "Optic to move a list of bounds to the right like rects.")
 (code
  (optic-compose
   list-traversal
   bounds<->rect
   rect-top-left-lens
   posn-x-lens)
  code:blank
  #,(target (list (bounds (posn left0 top0) (posn right0 bot0))
                  (bounds (posn left1 top1) (posn right1 bot1))
                  ...))
  <->
  (list (rect (posn #,(focus x0) y0) width0 height0)
        (rect (posn #,(focus x1) y1) width1 height1)
        ...)))

(slide
 #:title "Optics Review"
(para "Isomorphisms")
 (item "one \"focus\" that is a different representation of the target")
 (item "operations: forward, backward, modify")
 (para "Lenses")
 (item "one focus")
 (item "operations: get, modify")
 (para "Traversals")
 (item "zero or more foci")
 (item "operations: fold, modify"))

(slide
 #:title "Optics Review"
 (horiz (colorize (t "where") "blue") (t "vs") (colorize (t "what") "red"))
 (code
  (define (move-lob-to-right lob dx)
    (traversal-modify
     #,(frame (code (optic-compose
                     list-traversal
                     bounds<->rect
                     rect-top-left-lens
                     posn-x-lens))
         #:color "blue"
         #:line-width 3)
     lob
     #,(framed-code (lambda (x) (+ x dx)))))))

(slide
 #:title "Update Inspiration"
 (t "What if match could immutably update the target value?")
 (code
  > (update (rect (posn 1 2) 10 20)
      [(rect [top-left (posn x y)])
       (set! x (+ x 5))
       (set! y (+ y 9))])
  (rect (posn 6 11) 10 20)))

(slide
 #:title "How Match works"
 (t "Bind pattern variables to field values")
 (code
  (match p
    [(posn x _)
     <body>])
  ~>
  (let ([x (posn-x p)])
    <body>)))

(slide
 #:title "How Match works"
 (t "For nested patterns, use temporary variables")
 (code
  (match rct
    [(rect (posn x _) _ _)
     <body>])
  ~>
  (let ([tmp (rect-top-left rct)])
    (let ([x (posn-x tmp)])
      <body>))))

; I'll give you a hint: It has something to do with the
; things I've been talking about this whole time!

(slide
 #:title "How to Get Immutable Updates"
 (t "How can we get immutable updates?")
 'next
 (t "Bind variables to lenses!")
 (code
  (update p
    [(posn x)
     <body>])
  ~>
  (let ([x (struct-lens posn x)])
    <body>)))

(slide
 #:title "How to Get Immutable Updates"
 (t "For nested patterns, bind pattern variables to composed lenses.")
 (code
  (update rct
    [(rect [top-left (posn x)])
     <body>])
  ~>
  (let ([tmp (struct-lens rect top-left)])
    (let ([x (lens-compose tmp
                           (struct-lens posn x))])
      <body>))))

(slide
 #:title "How to Get Immutable Updates"
 (horiz (t "For ") (code set!) (t "and references in the body, we convert to lens operations."))
 (code
  (update rct
    [(rect [top-left (posn x)])
     (set! x (+ x 2))])
  ~>
  (let (...)
    (lens-set x rct (+ (lens-get x rct) 2)))))

(slide
 #:title "How to Get Immutable Updates"
 (t "It's not just lenses")
 (code
  (update posns
    [(list (posn x) ...)
     (modify! x (lambda (x) (+ x dx)))]))
 (horiz (t "Uses") (code list-traversal) (t "and") (code traversal-modify)))

(slide
 #:title "Tricky Syntactic Bits"
 (item "Special variables with custom" (code set!) "and reference behavior")
 (item "Recognizing struct name patterns and other built-in patterns")
 (item "Duplicate pattern variable check")
 (item "Macros for custom/new patterns?")
 (item "Other forms with special variable behavior like" (code modify!))
 'next
 (t "syntax-spec helps take care of all these features"))

(slide
 #:title "syntax-spec"
 (para "What you do")
 (item "Declaratively describe core grammar, binding rules, macro extensibility, syntactic sugar, entry-points")
 (item "Implement a compiler from core language to Racket (or other host)")

 (para "What you get")
 (item "Grammar and binding checks")
 (item "Macro extensibility for your DSL")
 (item "Binding-aware static analysis utilities")
 (item "Much more"))

(slide
 #:title "Implementation Overview"
 (item "Expander")
 (subitem "Grammar for patterns")
 (subitem "Macro extensibility and built-in syntactic sugar")
 (subitem "Binding structure")

 (item "Compiler")
 (subitem "Transform patterns into optics and let")
 (subitem "Transform variable usage in body and establish runtime context"))

(slide
 #:title "Patterns"
 (t "Grammar for patterns")
 (with-font "monaco"
   (vert
    (t "<pat> := _")
    (t "       | <var>")
    (t "       | (<struct-name> <pat> ...)")
    (t "       | (cons <pat> <pat>)")
    (t "       | (list <pat> ...)")
    (t "       | (list <pat> ... <pat> <ooo>)")
    (t "<ooo> := ..."))))

(slide
 #:title "Patterns"
 (t "Some patterns are equivalent to simpler ones")
 (code
  (list p1 p2 p3)
  ~>
  (cons p1 (cons p2 (cons p3 _)))))

(slide
 #:title "Patterns"
 (t "Most patterns compile to similar code")
 (code
  (update pair
    [(cons a _) <body>])
  ~>
  (let ([a car-lens]) <body>)
  code:blank
  code:blank
  (update psn
    [(posn x) <body>])
  ~>
  (let ([x (struct-lens posn x)]) <body>)))

(slide
 #:title "Patterns"
 (t "General optic pattern")
 (code
  (cons a _)
  ~>
  (optic car-lens a)
  code:blank
  code:blank
  (posn x)
  ~>
  (optic (struct-lens posn x) x)))

(slide
 #:title "Patterns"
 (t "Compilation of optic pattern")
 (code
  (update pair
    [(optic car-lens a) <body>])
  ~>
  (let ([a car-lens]) <body>)))

(slide
 #:title "Patterns"
 (t "Multiple sub-patterns")
 (code
  (update pair
    [(cons a b) <body>])
  ~>
  (let ([a car-lens]
        [b cdr-lens])
    <body>)))

(slide
 #:title "Patterns"
 (t "General and pattern")
 (code
  (cons a b)
  ~>
  (and (optic car-lens a)
       (optic cdr-lens b))))

(slide
 #:title "Patterns"
 (t "Core grammar for patterns")
 (with-font "monaco"
   (vert
    (t "<pat> := _")
    (t "       | <var>")
    (t "       | (optic <racket-expr> <pat>)")
    (t "       | (and <pat> ...)"))))


(slide
 #:title "Patterns"
 (t "syntax-spec grammar declaration for patterns")
 (code
  #,(hide-code
     (extension-class pattern-macro
                      #:binding-space pattern-update))
  (nonterminal/exporting pat
    #,(hide-code
       #:allow-extension pattern-macro
       #:binding-space pattern-update
       (~> (name:struct-id field ...)
           #'(struct* name field ...)))
    _
    v:pattern-var
    #,(hide-code #:binding (export v))
    (optic o:racket-expr p:pat)
    #,(hide-code #:binding (re-export p))
    (and2 p1:pat p2:pat)
    #,(hide-code #:binding [(re-export p1) (re-export p2)]))))

(slide
 #:title "Patterns"
 (t "syntax-spec grammar declaration for patterns")
 (code
  #,(hide-code
     (extension-class pattern-macro
                      #:binding-space pattern-update))
  (nonterminal/exporting pat
    #,(hide-code
       #:allow-extension pattern-macro
       #:binding-space pattern-update
       (~> (name:struct-id field ...)
           #'(struct* name field ...)))
    _
    v:pattern-var
    #:binding (export v)
    (optic o:racket-expr p:pat)
    #:binding (re-export p)
    (and2 p1:pat p2:pat)
    #:binding [(re-export p1) (re-export p2)])))

(slide
 #:title "Patterns"
 (t "syntax-spec grammar declaration for patterns")
 (code
  (extension-class pattern-macro
                   #,(hide-code #:binding-space pattern-update))
  (nonterminal/exporting pat
    #:allow-extension pattern-macro
    #,(hide-code
       #:binding-space pattern-update
       (~> (name:struct-id field ...)
           #'(struct* name field ...)))
    _
    v:pattern-var
    #:binding (export v)
    (optic o:racket-expr p:pat)
    #:binding (re-export p)
    (and2 p1:pat p2:pat)
    #:binding [(re-export p1) (re-export p2)])))

(slide
 #:title "Patterns"
 (t "syntax-spec grammar declaration for patterns")
 (code
  (extension-class pattern-macro
                   #:binding-space pattern-update)
  (nonterminal/exporting pat
    #:allow-extension pattern-macro
    #:binding-space pattern-update
    #,(hide-code
       (~> (name:struct-id field ...)
           #'(struct* name field ...)))
    _
    v:pattern-var
    #:binding (export v)
    (optic o:racket-expr p:pat)
    #:binding (re-export p)
    (and2 p1:pat p2:pat)
    #:binding [(re-export p1) (re-export p2)])))

(slide
 #:title "Patterns"
 (t "syntax-spec grammar declaration for patterns")
 (code
  (extension-class pattern-macro
                   #:binding-space pattern-update)
  (nonterminal/exporting pat
    #:allow-extension pattern-macro
    #:binding-space pattern-update
    (~> (name:struct-id field ...)
        #'(struct* name field ...))
    _
    v:pattern-var
    #:binding (export v)
    (optic o:racket-expr p:pat)
    #:binding (re-export p)
    (and2 p1:pat p2:pat)
    #:binding [(re-export p1) (re-export p2)])))

(slide
 #:title "Patterns"
 (t "Built-in Syntactic sugar")
 (code
  (define-dsl-syntax cons pattern-macro
    (syntax-rules ()
      [(cons a d)
       (and (optic car-lens a)
            (optic cdr-lens d))]))))

(slide
 #:title "Patterns"
 (t "Built-in Syntactic sugar")
 (code
  (define-dsl-syntax list pattern-macro
    (syntax-parser
      [(list)
       #'_]
      [(list p (~datum ...))
       #'(optic list-traversal p)]
      [(list p0 p ...)
       #'(cons p0 (list p ...))]))))

(slide
 #:title "Compiler"
 (t "Entry point")
 (code
  (host-interface/expression
    (update target:racket-expr
            [p:pat body:racket-expr])
    #:binding (scope (import p) body)
    #,(hide-code
       #'(let ([target-v target])
           (bind-optics identity-iso p
             (parameterize ([current-update-target
                             target-v])
               body)))))))

(slide
 #:title "Compiler"
 (t "Entry point")
 (code
  (host-interface/expression
    (update target:racket-expr
            [p:pat body:racket-expr])
    #:binding (scope (import p) body)
    #'(let ([target-v target])
        (bind-optics identity-iso p
          (parameterize ([current-update-target
                          target-v])
            body))))))

(slide
 #:title "Compiler"
 (t "Pattern compiler")
 (scale
  (code
   (define-syntax bind-optics
     (syntax-parser
       [(_ current-optic:id p body)
        (syntax-parse #'p
          #:literal-sets (pattern-literals)
          code:blank
          code:blank
          [var:id
           #'(let ([var current-optic])
               body)]
          [(optic o p)
           #'(let ([tmp-optic (optic-compose current-optic o)])
               (bind-optics tmp-optic p body))]
          [(and2 p1 p2)
           #'(bind-optics current-optic p1
               (bind-optics current-optic p2 body))])])))
  0.8))

(slide
 #:title "Compiler"
 #:layout 'top
 (para "Body compiler")
 (item "Establish runtime context")
 (item "Variable references and" (code set!))
 (item "Special forms like" (code modify!))
 )

(slide
 #:title "Compiler"
 #:layout 'top
 (para "Body compiler")
 (item "Establish runtime context")
 'next
 (para "We did this in the entry point")
 (code
  (parameterize ([current-update-target
                  target-v])
    body)))

(slide
 #:title "Compiler"
 #:layout 'top
 (para "Body compiler")
 (item "Variable references and" (code set!))
 (para "References use" (code lens-get) "and" (code set!) "uses" (code lens-set))
 'next
 (code
  (binding-class
   pattern-var
   #:reference-compiler pattern-var-reference-compiler)))

(slide
 #:title "Compiler"
 #:layout 'top
 (para "Body compiler")
 (item "Variable references and" (code set!))
 (para "References use" (code lens-get) "and" (code set!) "uses" (code lens-set))
 (code
  (define pattern-var-reference-compiler
    (make-variable-like-reference-compiler
     (syntax-parser
       [x:id #'(lens-get x (current-update-target))])
     (syntax-parser
       [(set! x val)
        #'(begin
            (current-update-target
             (lens-set x (current-update-target) val))
            (current-update-target))])))))

(slide
 #:title "Compiler"
 #:layout 'top
 (para "Body compiler")
 (item "Special forms like" (code modify!))
 (para "Get optic itself instead of value")
 (code
  > (update pair
      [(cons a b)
       (optic a)])
  car-lens))

(slide
 #:title "Compiler"
 #:layout 'top
 (para "Body compiler")
 (item "Special forms like" (code modify!))
 (para "Get optic itself instead of value")
 (para "Another \"entry point\"")
 (code
  (host-interface/expression
    (optic x:pattern-var)
    #'x))
 (para "Reference compiler will not transform the reference"))

(slide
 #:title "Compiler"
 #:layout 'top
 (para "Body compiler")
 (item "Special forms like" (code modify!))
 (para "use" (code traversal-modify))
 (code
  (define-syntax-rule (modify! x func)
    (current-update-target
     (traversal-modify (optic x)
                       (current-update-target)
                       func))
    (current-update-target))))

(slide
  (titlet "That's it!"))

(slide
 #:title "Acknowledgements"
 (item "Michael Ballantyne")
 (item "The Haskell Lens library")
 (item "The Racket community"))

(slide
 #:title "Ocular Patdown"
 (t "This library and DSL are available in")
 (code ocular-patdown))

;; TODO Acknowledge haskell lens library. you copy some docs like lens laws
