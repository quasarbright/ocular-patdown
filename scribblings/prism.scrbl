#lang scribble/manual

@require[
  scribble/example
  @for-label[
    racket
    ocular-patdown/optics/prism
    ocular-patdown/optics/traversal
    ocular-patdown/optics/lens
    ocular-patdown/optics
  ]
]
@(define op-eval (make-base-eval))
@examples[#:hidden #:eval op-eval (require (except-in racket set) ocular-patdown)]

@title{Prisms}

@defmodule[ocular-patdown/optics/prism]

A @deftech{prism} is an @tech{optic} that is like an @tech{isomorphism}, but can be partial
in one direction. In terms of @tech[#:key "focus"]{foci}, it can have zero or one. It is useful for dealing with subtypes, or situations where you can convert from
A to B, but not always from B to A.

@examples[
  #:eval op-eval
  (prism-project string-number-prism "1")
  (prism-inject string-number-prism 1)
  (prism-project string-number-prism "one" #f)
]

All @tech{isomorphism}s are prisms, but not all prisms are isomorphisms.

All prisms are @tech{traversal}s, but not all traversals are prisms.

@defproc[(prism? [val any/c]) boolean?]{
  Predicate for prisms. Recognizes implementers of @racket[gen:prism].
  @examples[
    #:eval op-eval
    (prism? string-number-prism)
    (prism? symbol<->string)
    (prism? car-lens)
    (prism? list-traversal)
  ]
}

@defproc[(make-prism [project (-> any/c (or/c any/c prism-absent?) any/c)] [inject (-> any/c any/c)]) prism?]{
  Constructor for prisms. @racket[project] takes in a @tech{target} and returns the @tech{focus} if there is one, or @racket[(prism-absent)] otherwise.
  @racket[inject] takes in a focus and returns a target.
  @examples[
    #:eval op-eval
    (define my-string-number-prism
      (make-prism
        (lambda (str) (or (string->number str) (prism-absent)))
        number->string))
  ]
  There are a few laws that prisms should obey:
  @itemize[
    @item{
      Projecting after injecting should yield the injected focus.
      @racketblock[(equal? (project (inject focus)) focus)]
    }
    @item{
      If the target has a focus, injecting after projecting should yield the target.
      @racketblock[(equal? (inject (project target)) target)]
    }
  ]
  These laws are the same as the isomorphism laws (see @racket[make-iso]) when there is a focus.
  In other words, @racket[project] and @racket[inject] must be inverse functions of each other when there is a focus.
}

@defproc[(prism-absent) prism-absent?]{
  Constructs a value representing the absence of a focus in a prism.
}

@defproc[(prism-absent? [val any/c]) boolean?]{
  Predicate for @racket[prism-absent].
}

@section{Prism Operations}

@defproc[(prism-project [prism prism?] [target any/c] [failure-result (or/c any/c (-> any/c)) (lambda () (error ...))]) any/c]{
  Get the focus of @racket[target] under @racket[prism] if there is one, and use @racket[failure-result] otherwise.

  @racket[failure-result] is either a value or a zero-argument procedure that returns a value.
  @examples[
    #:eval op-eval
    (prism-project string-number-prism "1")
    (prism-project string-number-prism "one" #f)
  ]
}

@defproc[(prism-inject [prism prism?] [focus any/c]) any/c]{
  Get the target from @racket[focus] under @racket[prism].
  @examples[
    #:eval op-eval
    (prism-inject string-number-prism 1)
  ]
}

@defproc[(prism-compose [prism prism?] ...) prism?]{
  Like @racket[optic-compose], but for prisms.
  @examples[
    #:eval op-eval
    (prism-inject (prism-compose symbol<->string string-number-prism) 1)
  ]
}

@section{Library Prisms}

@defthing[string-number-prism prism?]{
  Prism between strings and numbers.
  @examples[
    #:eval op-eval
    (prism-project string-number-prism "1")
    (prism-project string-number-prism "one" #f)
    (prism-inject string-number-prism 1)
  ]
}

@defproc[(guard-prism [predicate (-> any/c boolean?)]) prism?]{
  Prism that has a focus if @racket[(predicate target)] is true.
  @examples[
    #:eval op-eval
    (prism-project (guard-prism number?) 1)
    (prism-project (guard-prism number?) "one" #f)
    (prism-inject (guard-prism number?) 1)
  ]
}

@section{Prism Generic Interface}


@defidform[gen:prism]{
  Generic interface for prisms.
  @examples[
    #:eval op-eval
    (struct make-prism (project inject)
      #:methods gen:prism
      [(define (prism-project prism target [failure-result default-failure-result])
         (let ([focus ((make-prism-project prism) target)])
           (if (prism-absent? focus)
               (if (procedure? failure-result)
                   (failure-result)
                   failure-result)
               focus)))
       (define (prism-inject prism focus)
         ((make-prism-inject prism) focus))])
  ]
}
