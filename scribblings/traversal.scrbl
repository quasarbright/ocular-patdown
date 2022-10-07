#lang scribble/manual

@require[scribble/example @for-label[racket ocular-patdown/optics/traversal ocular-patdown/optics/lens ocular-patdown/optics]]
@(define op-eval (make-base-eval))
@examples[#:hidden #:eval op-eval (require (except-in racket set) ocular-patdown)]

@title{Traversals}

@defmodule[ocular-patdown/optics/traversal]

A @deftech{traversal} is an @tech{optic} that can have zero or more @tech[#:key "focus"]{foci}. Updating with a traversal is like using @racket[map].

@examples[
  #:eval op-eval
  (traversal-modify list-traversal (list 1 2 3) add1)
  (traversal-foldl list-traversal (list 1 2 3) cons '())
]

All @tech{lens}es are traversals, but not all traversals are lenses.

@defproc[(traversal? [val any/c]) boolean?]{
  Predicate for traversals. Recognizes implementers of @racket[gen:traversal].
  @examples[
    #:eval op-eval
    (traversal? list-traversal)
    (traversal? car-lens)
  ]
}

@defproc[(make-traversal [map-proc (-> (-> any/c any/c) any/c any/c)]
                         [fold-proc (-> (-> any/c any/c any/c) any/c any/c any/c)])
         traversal?]{
  Constructor for traversals. Takes in a function like @racket[map] and a function like @racket[foldl], but the functions should operate on the foci within the target.
  @examples[
    #:eval op-eval
    (define my-list-traversal (make-traversal map foldl))
    (define my-maybe-traversal
      (make-traversal
        (lambda (proc val) (if val (proc val) val))
        (lambda (proc init val) (if val (proc val init) init))))
  ]
  There are a few laws traversals should obey:
  @itemize[
    @item{@racket[traversal-modify]ing a target under a traversal should leave it with the same number of foci.}
    @item{@racket[traversal-foldl] should not call the folding procedure with the same focus twice.}
  ]
  Like lenses, traversals should be pure.
}

@section{Traversal Operations}

@defproc[(traversal-modify [traversal traversal?] [target any/c] [proc (-> any/c any/c)]) any/c]{
  Apply @racket[proc] to each focus of @racket[target] under @racket[traversal]. Like @racket[map].
  @examples[
    #:eval op-eval
    (traversal-modify list-traversal (list 1 2 3) add1)
    (traversal-modify vector-traversal (vector 1 2 3) sub1)
  ]
}

@defproc[(traversal-foldl [traversal traversal?] [target any/c] [proc (-> any/c any/c any/c)] [init any/c]) any/c]{
  @racket[foldl] over the foci of @racket[target] under @racket[traversal].
  @examples[
    #:eval op-eval
    (traversal-foldl list-traversal (list 1 2 3) + 0)
    (traversal-foldl vector-traversal (vector 1 2 3) cons '())
  ]
}

@defproc[(traversal->list [traversal traversal?] [target any/c]) (listof any/c)]{
  Get a list of all foci of @racket[target] under @racket[traversal].
  @examples[
     #:eval op-eval
     (traversal->list vector-traversal (vector 1 2 3 4))
  ]
}

@defproc[(traversal-compose [traversal traversal?] ...) traversal?]{
  Compose traversals like @racket[optic-compose].
  @examples[
    #:eval op-eval
    (define lol-traversal (traversal-compose list-traversal list-traversal))
    (traversal-modify lol-traversal '((1 2 3) () (4)) add1)
    (traversal-modify (traversal-compose list-traversal maybe-traversal car-lens)
                      '(#f (10 20 30) #f (40 50))
                      sqr)
  ]
}

@section{Library Traversals}

@defthing[list-traversal traversal?]{
  Traversal that focuses on each element of a list.
  @examples[
    #:eval op-eval
    (traversal-modify list-traversal (list 1 2 3) add1)
    (traversal-foldl list-traversal (list 1 2 3) cons '())
  ]
}

@defthing[vector-traversal traversal?]{
  Traversal that focuses on each element of a vector.
   @examples[
    #:eval op-eval
    (traversal-modify vector-traversal (vector 1 2 3) add1)
    (traversal-foldl vector-traversal (vector 1 2 3) cons '())
  ]
}

@defthing[rose-traversal traversal?]{
  Traversal that focuses on each leaf of a rose tree, where a rose tree is either a non-list or a list of rose trees.

  Raises an exception when modification results in a focus turning into a list since this could lead to the number of targets changing.

  @examples[
    #:eval op-eval
    (traversal-modify rose-traversal '((1 2) () ((3) 4)) add1)
    (traversal-foldl rose-traversal '(("a" "b") "c") string-append "")
  ]
}

@defthing[maybe-traversal traversal?]{
  Traversal that focuses on the target if it is not @racket[#f], and has no focus otherwise.
  Useful to compose with other optics to get optional behavior.
  @examples[
    #:eval op-eval
    (traversal-modify maybe-traversal 1 add1)
    (traversal-modify maybe-traversal #f add1)
    (traversal->list maybe-traversal 1)
    (traversal->list maybe-traversal #f)
    (traversal-modify (traversal-compose list-traversal maybe-traversal car-lens)
                      '(#f (10 20 30) #f (40 50))
                      sqr)
  ]
}

@section{Generic Traversal Interface}

@defidform[gen:traversal]{
  A generic interface for traversals.
  @examples[
    #:eval op-eval
    (struct make-traversal (map foldl)
      #:methods gen:traversal
      [(define (traversal-modify t target proc)
         ((make-traversal-map t) proc target))
       (define (traversal-foldl t target proc init)
         ((make-traversal-foldl t) proc init target))])
  ]
}
