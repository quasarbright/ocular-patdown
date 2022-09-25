#lang scribble/manual

@require[scribble/example @for-label[racket ocular-patdown]]
@(define op-eval (make-base-eval))
@examples[#:hidden #:eval op-eval (require (except-in racket set) ocular-patdown)]

@title[#:tag "optics-guide"]{Optics Guide}

This page serves as a guide for those who aren't familiar with @tech{optic}s.

Optics are useful for performing accesses and immutable updates deeply within a structure. They are very powerful,
but they are also very abstract and have a bit of a learning curve.

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

@examples[
  #:eval op-eval
  (struct rect [top-left width height])
  (define rect1 (rect (posn 10 20) 16 9))
  (define rect-top-left-lens (struct-lens rect top-left))
  (define rect-x-lens (lens-compose rect-top-left-lens posn-x-lens))
  (lens-get rect-x-lens rect1)
  (lens-set rect-x-lens rect1 35)

  (struct tree [val children])
  (define tree1 (tree 1 (list (tree 2 (list (tree 3 '()))) (tree 4 '()))))
  (define tree-children-lens (struct-lens tree children))
  (define tree-value-lens (struct-lens tree val))
  (define tree-first-value-lens (lens-compose tree-children-lens car-lens tree-value-lens))
  (lens-get tree-first-value-lens tree1)
  (lens-set tree-first-value-lens tree1 20)
]

When we compose lenses, the focus of the first is used as the target of the second. Using lens composition, we can perform very deep accesses
and modifications.
