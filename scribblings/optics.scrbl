#lang scribble/manual

@require[scribble/example @for-label[(except-in racket set) ocular-patdown/optics/lens ocular-patdown/optics ocular-patdown/update]]
@(define op-eval (make-base-eval))
@examples[#:hidden #:eval op-eval (require (except-in racket set) ocular-patdown)]

@title{Optics}

@defmodule[ocular-patdown/optics]

Under the hood, the @racket[update] form uses @deftech{optic}s. An optic is a first class getter and (immutable) setter for some @deftech{target} type and a @deftech{focus} or several foci within it.
For example, @racket[car-lens] is an optic that targets pairs and focuses on a pair's @racket[car]. This optic can be used to get the @racket[car] of a pair, or return a new pair
with an updated @racket[car].

@examples[
  #:eval op-eval
  (define pair (cons 1 2))
  (optic-get car-lens pair)
  (optic-set car-lens pair 3)
  pair
]

Optics can be composed to focus on values deep within a structure.

@examples[
  #:eval op-eval
  (define pair (cons 1 (cons 2 3)))
  (optic-set (optic-compose cdr-lens car-lens) pair #t)
]

@defproc[(optic? [val any/c]) boolean?]{
  Predicate for optics.
  @examples[
    #:eval op-eval
    (optic? car-lens)
    (optic? list-traversal)
    (optic? (cons 1 2))
  ]
}

@defproc[(optic-get [optic lens?] [target any/c]) any/c]{
  Gets the @tech{focus} of @racket[target] under @racket[optic]. The optic must be a @tech{lens}.

  @examples[
    #:eval op-eval
    (optic-get car-lens (cons 1 2))
    (optic-get cdr-lens (cons 1 2))
  ]
}

@defproc[(optic-set [optic lens?] [target any/c] [focus any/c]) any/c]{
  Sets the focus of @racket[target] to @racket[focus] under @racket[optic]. Does not mutate @racket[target], but rather, returns an updated copy of it. The optic must be a @tech{lens}.
  @examples[
    #:eval op-eval
    (optic-set car-lens (cons 1 2) #t)
  ]
}

@defproc[(optic-modify [optic optic?] [target any/c] [proc (-> any/c any/c)]) any/c]{
  Like @racket[optic-set], but applies a function to the current value of the focus to update it. For multiple foci, each focus is updated individually. The optic does not have to be a @tech{lens}.
  @examples[
    #:eval op-eval
    (optic-modify car-lens (cons 2 3) sqr)
    (optic-modify list-traversal (list #t #t #f) not)
  ]
}

@defproc[(optic-compose [optic optic?] ...) optic?]{
  Compose multiple optics where the focus of the first optic is the target of the next. In other words, the shallow, top-level optic comes first and the deep, bottom-level optic comes last.
  @examples[
    #:eval op-eval
    (struct tree [val children] #:transparent)
    (define tree-first-child-lens (optic-compose (struct-lens tree children) car-lens))
    (optic-set tree-first-child-lens
               (tree 1 (list (tree 2 '()) (tree 3 '())))
               (tree #t '()))
    (define second-lens (optic-compose cdr-lens car-lens))
    (optic-set second-lens (list 1 2) #t)
    (define first-of-second-lens (optic-compose second-lens car-lens))
    (optic-set first-of-second-lens (list 1 (list 2 3)) #t)
    (define first-of-each-traversal (optic-compose list-traversal car-lens))
    (optic-modify first-of-each-traversal
               (list (list 1 2 3) (list 4 5) (list 6))
               number->string)
  ]

  When composing optics of different types, the result is an instance of the most general optic type among the arguments.
  @examples[
    #:eval op-eval
    (lens? (optic-compose car-lens car-lens))
    (lens? (optic-compose list-traversal car-lens))
    (traversal? (optic-compose list-traversal car-lens))
  ]
}
