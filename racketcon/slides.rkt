#lang slideshow

(require slideshow/code)

(slide
 (titlet "A Match-Like DSL for Deep Immutable Updates")
 (titlet "Mike Delmonaco"))

(slide
 #:title "Agenda"
 (item "Lenses and Other Optics")
 (item "syntax-spec")
 (item "update DSL"))

(begin
  ; needed to do multiple slides instead of 'next bc of centering issues with multiple uses of code
  (slide
   #:title "A Sneak Peek"
   #:layout 'top
   (code
    (struct posn [x y] #:transparent)
    (struct rect [top-left width height] #:transparent)
    (define rct (rect (posn 0 0) 10 20))))

  (slide
   #:title "A Sneak Peek"
   #:layout 'top
   (code
    (struct posn [x y] #:transparent)
    (struct rect [top-left width height] #:transparent)
    (define rct (rect (posn 0 0) 10 20))
    code:blank
    (code:comment "move to the right by 3")
    > (update rct
        [(rect [top-left (posn [x])])
         (set! x (+ x 3))])
    (rect (posn 3 0) 10 20)))

  (slide
   #:title "A Sneak Peek"
   #:layout 'top
   (code
    (struct posn [x y] #:transparent)
    (struct rect [top-left width height] #:transparent)
    (define rct (rect (posn 0 0) 10 20))
    code:blank
    (code:comment "move to the right by 3")
    > (update rct
        [(rect [top-left (posn [x])])
         (set! x (+ x 3))])
    (rect (posn 3 0) 10 20)
    code:blank
    (code:comment "did not mutate rct!")
    > rct
    (rect (posn 0 0) 10 20))))

(slide
 #:title "Moving Rectangles"
 (code
  (struct posn [x y] #:transparent)
  (struct rect [top-left width height] #:transparent)
  (define rct (rect (posn 0 0) 10 20))
  code:blank
  (code:comment "rect? number? -> rect?")
  (code:comment "move the rectangle to the right")
  (define (rect-move-to-right rct dx)
    (struct-copy
     rect rct
     [top-left (posn-move-to-right (rect-top-left rct)
                                   dx)]))
  code:blank
  (code:comment "posn? number? -> rectangle?")
  (code:comment "move the posn to the right")
  (define (posn-move-to-right pos dx)
    (struct-copy posn pos [x (+ (posn-x pos) dx)]))))

(slide
 #:title "Moving Rectangles"
 (code
  (code:comment "rect? number? -> rect?")
  (code:comment "move the rectangle to the left")
  (define (rect-move-to-left rct dx)
    (struct-copy
     rect rct
     [top-left (posn-move-to-left (rect-top-left rct)
                                  dx)]))
  code:blank
  (code:comment "posn? number? -> rectangle?")
  (code:comment "move the posn to the left")
  (define (posn-move-to-left pos dx)
    (struct-copy posn pos [x (- (posn-x pos) dx)]))))

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
 )

(slide
 #:title "Modifiers"
 (code
  (code:comment "rect? (posn? -> posn?) -> rect?")
  (define (rect-modify-top-left rct proc)
    (struct-copy
     rect rct
     [top-left (#,(framed-code proc) (rect-top-left rct))]))))

(slide
 #:title "Modifiers"
 (code
  (code:comment "rect? (posn? -> posn?) -> rect?")
  (define (rect-modify-top-left rct proc)
    (struct-copy
     rect rct
     [top-left (#,(framed-code proc) (rect-top-left rct))]))
  code:blank
  (code:comment "posn? (number? -> number?) -> posn?")
  (define (posn-modify-x pos proc)
    (struct-copy posn pos [x (proc (posn-x pos))]))))

(slide
 #:title "Modifiers"
 (code
  (code:comment "rect? (posn? -> posn?) -> rect?")
  (define (rect-modify-top-left rct proc)
    (struct-copy
     rect rct
     [top-left (#,(framed-code proc) (rect-top-left rct))]))
  code:blank
  (code:comment "posn? (number? -> number?) -> posn?")
  (define (posn-modify-x pos proc)
    (struct-copy
     posn pos
     [x (#,(framed-code proc) (posn-x pos))]))))

(slide
 #:title "Modifiers"
 (code
  (define (rect-move-to-right rct dx)
    (rect-modify-top-left
     rct
     (lambda (top-left)
       (posn-modify-x
        top-left
        (lambda (x) (+ x dx))))))))

(slide
 #:title "Modifiers"
 (code
  (define (rect-move-to-right rct dx)
    (rect-modify-top-left
     rct
     (lambda (top-left)
       (posn-modify-x
        top-left
        #,(framed-code (lambda (x) (+ x dx)))))))))

(slide
 #:title "Modifiers"
 (code
  (code:comment "rect? (number? -> number?) -> rect?")
  (define (rect-modify-x rct proc)
    (rect-modify-top-left
     rct
     (lambda (top-left)
       (posn-modify-x
        top-left
        #,(framed-code proc)))))))

(slide
 #:title "Modifiers"
 (code
  (code:comment "A (Modifier Target Focus) is a")
  (code:comment "(Target (Focus -> Focus) -> Target)")
  (code:comment "Represents an immutable updater.")
  (code:comment "The Target is the overall structure.")
  (code:comment "The Focus is the piece of the target")
  (code:comment "that gets updated.")
  (code:comment "Examples:")
  (code:comment "rect-modify-top-left is a (Modifier Rect Posn)")
  (code:comment "posn-modify-x is a        (Modifier Posn Number)")
  (code:comment "rect-modify-x is a        (Modifier Rect Number)")))

(slide
 #:title "Modifiers"
 (scale
  (code
   (code:comment "(Modifier rect? number?)")
   (define (rect-modify-x rct proc)
     (#,(framed-code rect-modify-top-left)
      rct
      (lambda (top-left)
        (#,(framed-code posn-modify-x)
         top-left
         proc))))
   code:blank
   (code:comment "(Modifier A B) (Modifier B C) -> (Modifier A C)")
   (code:comment "The focus of the first modifier (B)")
   (code:comment "is the target of the second")
   (define (modifier-compose mod-a-b mod-b-c)
     (lambda (a proc)
       (#,(framed-code mod-a-b)
        a
        (lambda (b)
          (#,(framed-code mod-b-c)
           b
           proc)))))
   code:blank
   (define rect-modify-x
     (modifier-compose rect-modify-top-left posn-modify-x)))
  0.7))

(slide
 #:title "Lenses"
 ;; TODO
 )
