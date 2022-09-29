#lang scribble/manual

@require[scribble/example @for-label[racket ocular-patdown]]
@(define op-eval (make-base-eval))
@examples[#:hidden #:eval op-eval (require racket ocular-patdown)]

@title[#:tag "optics-guide"]{Optics Guide}

This page serves as a guide for those who aren't familiar with @tech{optic}s.

Optics are useful for performing accesses and immutable updates deeply within a structure. They are very powerful,
but they are also very abstract and have a bit of a learning curve.

The big idea is that optics allow you to separate the "where" from the "what" when you are operating on parts of a
structure. An optic describes "where", and something like a procedure may describe "what" you want to do.
Additionally, optics provide a rich language of specifying "where", and are re-usable.

@section{Lenses}

The most intuitive type of optic is a lens. A lens is a first class getter and setter for a focus within a target.

For example,
@examples[
  #:eval op-eval
  #:label #f
  (struct posn [x y] #:transparent)
  (define posn-x-lens (struct-lens posn x))
]

@racket[posn-x-lens] is a lens that targets a @racket[posn] and focuses on its @racket[x] field. We can use
this lens to get a @racket[posn]'s @racket[x] value or to make a copy of a @racket[posn] with a new @racket[x] value:

@examples[
  #:eval op-eval
  #:label #f
  (define posn-1-2 (posn 1 2))
  (lens-get posn-x-lens posn-1-2)
  (lens-set posn-x-lens posn-1-2 10)
  posn-1-2
]

Using @racket[lens-set] does not mutate the target, it returns a modified copy of the target.

Here are other examples of lenses:

@; TODO more examples once there are more library lenses
@examples[
  #:eval op-eval
  #:label #f
  (lens-set car-lens (cons 1 2) #t)
  (lens-set cdr-lens (cons 1 2) #t)
]

These lenses focus on the @racket[car] and @racket[cdr] of a pair, respectively.

You can create your own lenses with @racket[make-lens].

@examples[
  #:eval op-eval
  (define my-car-lens (make-lens car (lambda (pair new-car) (cons new-car (cdr pair)))))
  (lens-set my-car-lens (cons 1 2) #t)
]

You can also compose lenses to focus on values deep within a target.

@elemtag["rect-example"]{ }
@examples[
  #:eval op-eval
  (struct rect [top-left width height] #:transparent)
  (define rect1 (rect (posn 10 20) 16 9))
  (define rect-top-left-lens (struct-lens rect top-left))
  (define rect-x-lens (lens-compose rect-top-left-lens posn-x-lens))
  (lens-get rect-x-lens rect1)
  (lens-set rect-x-lens rect1 35)

  (struct tree [val children] #:transparent)
  (define tree1 (tree 1 (list (tree 2 (list (tree 3 '()))) (tree 4 '()))))
  (define tree-children-lens (struct-lens tree children))
  (define tree-value-lens (struct-lens tree val))
  (define tree-first-value-lens (lens-compose tree-children-lens car-lens tree-value-lens))
  (lens-get tree-first-value-lens tree1)
  (lens-set tree-first-value-lens tree1 20)
]

When we compose lenses, the focus of the first is used as the target of the second. Using lens composition, we can perform very deep accesses
and modifications.

@section{Traversals}

Traversals are like lenses, except they can have zero or multiple @tech[#:key "focus"]{foci}. Traversals are useful for focusing on all the elements
of a collection. A traversal is like a first class @racket[map] and a @racket[foldl]{fold}.

@examples[
  #:eval op-eval
  (traversal-modify list-traversal '(1 2 3) add1)
  (traversal-modify rose-traversal '((1 2) (3 ((4)))) add1)
  (traversal-foldl rose-traversal '((1 2) (3 ((4)))) + 0)
  (traversal->list rose-traversal '((1 2) (3 ((4)))))
]

All lenses are traversals, but not all traversals are lenses. Lenses are just traversals that happen to have exactly
one target.

@examples[
  #:eval op-eval
  (traversal? car-lens)
  (traversal-modify car-lens '(1 2) number->string)
  (lens? list-traversal)
]

Like lenses, traversals can be composed. Each focus of the first traversal becomes the target for the second traversal.
All the inner foci of all the outer targets are foci of the composition.

@examples[
  #:eval op-eval
  (define lol-traversal (traversal-compose list-traversal list-traversal))
  (traversal-modify lol-traversal '((1 2 3) (4) ()) add1)
]

Where things really get interesting is when we compose lenses and traversals.

@examples[
  #:eval op-eval
  (define lop-x-traversal (traversal-compose list-traversal posn-x-lens))
  (traversal? lop-x-traversal)
  (traversal-modify lop-x-traversal (list (posn 10 20) (posn 30 40)) sqr)
  (define tree-child-value-traversal (traversal-compose tree-children-lens list-traversal tree-value-lens))
  (traversal? tree-child-value-traversal)
  (traversal-modify tree-child-value-traversal tree1 number->string)
]

When we compose a traversal with a lens, we get a traversal. Since lenses are traversals that happen to have a single target,
they work just fine with traversal composition. This, along with adding other optics into the mix, can be used to express a wide
variety of computations. Composing simple optics in this way allows you to focus on pretty much any values you'd want to in a target.

You can create recursive traversals that refer to themselves.

@examples[
  #:eval op-eval
(define tree-children-traversal (optic-compose tree-children-lens list-traversal))
(define tree-values-traversal
  (make-traversal
   (λ (proc tree) (traversal-modify (traversal-compose tree-children-traversal tree-values-traversal) (lens-modify tree-value-lens tree proc) proc))
   (λ (proc init tree) (traversal-foldl (traversal-compose tree-children-traversal tree-values-traversal) tree proc (proc (lens-get tree-value-lens tree) init)))))
(traversal-modify tree-values-traversal tree1 number->string)
(traversal-foldl tree-values-traversal tree1 cons '())
]

@;TODO once you have a better way to write recursive optics, put an example here

@section{Isomorphisms}

An isomorphism is a lens where the @tech{focus} is "equivalent" to the @tech{target}.
It is used when two types or representations of data can be converted back and forth between each other.
Isomorphisms are useful for treating data from one representation as another, like an adapter.

@examples[
  #:eval op-eval
  (iso-modify symbol<->string 'foo string-upcase)
]

Here, we are using the isomorphism between symbols and strings to treat a symbol as a string. We are using a
string-to-string function, but we are inputting and outputting a symbol. The isomorphism, @racket[symbol<->string] just
automates this back-and-forth conversion.

In math, an isomorphism is a one-to-one mapping between two sets. In code, we
represent this as a pair of functions which are inverses of each other.

@examples[
  #:eval op-eval
  (define my-symbol<->string (make-iso symbol->string string->symbol))
  my-symbol<->string
]

All isomorphisms are lenses (and thus, are traversals too), but not all lenses are isomorphisms.

@examples[
  #:eval op-eval
  (lens? symbol<->string)
  (lens-get symbol<->string 'chocolate)
  (lens-set symbol<->string 'cookies-and-cream "oreo")
]

Think about that usage of @racket[lens-set]. For an isomorphism, there is no need to supply the target!
This is because the new focus completely determines the new target. As such, the library provides
@racket[iso-forward] and @racket[iso-backward], which correspond to @racket[lens-get] and @racket[lens-set] respectively.

@examples[
  #:eval op-eval
  (iso-forward symbol<->string 'chocolate)
  (iso-backward symbol<->string "oreo")
]

Like traversals, isomorphisms become very useful when combined with other optics. For example, let's consider
the following data structure representing a rectangular bounding box:

@examples[
  #:eval op-eval
  #:label #f
  (struct bounds [top-left bottom-right] #:transparent)
]

A @racket[bounds] represents a rectangular area of space. Recall the @elemref["rect-example"]{rectangle example} above.
These two data types are isomorphic:

@examples[
  #:eval op-eval
  #:label #f
  (define bounds<->rect
    (make-iso
     (lambda (b)
       (match b
         [(bounds (and top-left (posn x0 y0)) (posn x1 y1))
          (rect top-left (- x1 x0) (- y1 y0))]))
     (lambda (r)
       (match r
         [(rect (and top-left (posn x y)) width height)
          (bounds top-left (posn (+ x width) (+ y height)))]))))
  rect1
  (iso-backward bounds<->rect rect1)
  (iso-forward bounds<->rect (iso-backward bounds<->rect rect1))
]

We can use this isomorphism to treat a @racket[bounds] as a @racket[rect] and vice versa.

@examples[
  #:eval op-eval
  #:label #f
  (define rect-width-lens (struct-lens rect width))
  (define bound-width-lens (lens-compose bounds<->rect rect-width-lens))
  (define (widen-bounds bnd dw)
    (lens-modify bound-width-lens
                 bnd
                 (lambda (w) (+ w dw))))
  (widen-bounds (bounds (posn 10 10) (posn 20 20)) 5)
]

Here, we take advantage of the fact that all isomorphisms are lenses and compose our isomorphism with
the @racket[rect-width-lens]. This creates an optic that focuses on the width of a @racket[bounds], even
though a @racket[bounds] doesn't actually have a width field! Using this, we can treat a @racket[bounds]
as if it has a width and modify its width.

@examples[
  #:eval op-eval
  #:label #f
  (define/match (is-in-bounds? bnd pos)
    [((bounds (posn x0 y0) (posn x1 y1)) (posn x y))
     (and (<= x0 x x1) (<= y0 y y1))])
  (is-in-bounds? (bounds (posn 10 10) (posn 20 20)) (posn 15 15))
  (define (is-in-rect? rct pos)
    (is-in-bounds? (iso-backward bounds<->rect rct) pos))
  (is-in-rect? (rect (posn 10 10) 5 5) (posn 11 11))
]

Here, we have a predicate defined for a @racket[bounds], which is a representation that is suitable for bounds checking.
If we want to create the same predicate for rectangles, we can use our isomorphism to treat a @racket[rect] as a @racket[bounds].

@examples[
  #:eval op-eval
  #:label #f
  (define rect-bottom-right-lens (lens-compose (iso-reverse bounds<->rect) (struct-lens bounds bottom-right)))
  (lens-set rect-bottom-right-lens rect1 (posn 30 30))
]

Here, we create a lens that focuses on the bottom right position of a @racket[rect] even though it doesn't have one as a field.
The way we defined our isomorphism is useful for treating a @racket[bounds] as a @racket[rect], but here, we want to to the reverse.
Luckily, since isomorphisms are bi-directional, we can use @racket[iso-reverse] to treat a @racket[rect] as a @racket[bounds] in an
optic composition.

An isomorphism is like an adapter. If you have two equivalent representations, and need to perform operations on one that are easier on
the other, isomorphisms help you avoid explicitly converting back and forth. And when combined with optic composition, isomorphisms can
allow you to abstract these operations further.


Now let's put it all together. Let's create a function that increments the height of all @racket[bounds]s in a list of @racket[bounds]:

@examples[
  #:eval op-eval
  #:label #f
  (define (increment-bounds-heights lob)
    (traversal-modify
     (traversal-compose list-traversal bounds<->rect (struct-lens rect height))
     lob
     add1))
  (increment-bounds-heights (list (bounds (posn 0 0) (posn 10 10)) (bounds (posn 5 5) (posn 15 20))))
]

We use @racket[list-traversal] for its mapping behavior, we treat all the elements as @racket[rect]s, and we focus on their heights.
Then we just apply @racket[add1] to them.

@section{Conclusion}

Optics allow you to separate the "where" from the "what". You can create combinations of optics to specify exactly where
you want a modification or access, and then separately, you can specify what you want to do by providing a procedure like @racket[add1].

Traversals are useful for focusing on multiple parts of a structure, isomorphisms are useful for conversions/adapters between
equivalent representations of data, and lenses are useful for focusing on one particular part of a structure.

@;TODO mention traversal-append and conditional optics
There are other types of optics, like @tech{prism}s, and other ways of combining optics, like @racket[optic-compose] and @racket[update].
Read @seclink["reference"]{the reference} to find out more.
