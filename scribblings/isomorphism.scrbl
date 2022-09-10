#lang scribble/manual

@require[
  scribble/example
  @for-label[
    racket
    ocular-patdown/optics/isomorphism
    ocular-patdown/optics
  ]
]
@(define op-eval (make-base-eval))
@examples[#:hidden #:eval op-eval (require (except-in racket set) ocular-patdown)]

@title{Isomorphisms}

@defmodule[ocular-patdown/optics/isomorphism]

An @deftech{isomorphism} is an optic whose @tech{focus} is "equivalent" (isomorphic) to its @tech{target}.
An isomorphism can be used when two data types can be converted back and forth between each other.
Isomorphisms are useful for treating one data type as another.

@examples[
  #:eval op-eval
  (optic-modify symbol<->string 'foo string-upcase)
  (optic-get symbol<->string 'foo)
  (iso-forward symbol<->string 'foo)
  (iso-backward symbol<->string "foo")
]

All isomorphisms are @tech{lens}es, but not all lenses are isomorphisms.

@defproc[(iso? [val any/c]) boolean?]{
  Predicate for isomorphisms. Recognizes implementers of @racket[gen:iso].
  @examples[
    #:eval op-eval
    (iso? symbol<->string)
    (iso? car-lens)
  ]
}

@defproc[(make-iso [forward (-> any/c any/c)] [backward (-> any/c any/c)]) iso?]{
  Constructor for isomorphisms.
  @examples[
    #:eval op-eval
    (define my-symbol<->string (make-iso symbol->string string->symbol))
  ]

   @racket[forward] and @racket[backward] must be inverses of each other:
   @racketblock[(equal? (backward (forward target)) target)
                (equal? (forward (backward focus)) focus)]
}

@section{Isomorphism operations}

@defproc[(iso-forward [iso iso?] [target any/c]) any/c]{
  Convert @racket[target] to its equivalent focus.
  @examples[
    #:eval op-eval
    (iso-forward symbol<->string 'foo)
  ]
}

@defproc[(iso-backward [iso iso?] [focus any/c]) any/c]{
  Convert @racket[focus] to its equivalent target.
  @examples[
    #:eval op-eval
    (iso-backward symbol<->string "foo")
  ]
}

@defproc[(iso-modify [iso iso?] [target any/c] [proc (-> any/c any/c)]) any/c]{
  Modify @racket[target] with @racket[proc] as if it was its focus under @racket[iso].
  @racket[proc] must accept and return a focus.
  @examples[
    #:eval op-eval
    (iso-modify symbol<->string 'foo string-upcase)
  ]
}

@defproc[(iso-reverse [iso iso?]) iso?]{
  Reverse/invert @racket[iso] such that it works in the opposite direction.
  @examples[
    #:eval op-eval
    (iso-forward symbol<->string 'foo)
    (iso-forward (iso-reverse symbol<->string) "foo")
  ]
}

@defproc[(iso-compose [iso iso?] ...) iso?]{
  Like @racket[optic-compose], but for isomorphisms.
  @examples[
    #:eval op-eval
    (iso-forward (iso-compose vector<->list list-reverse-iso) #(1 2 3))
    (iso-backward (iso-compose vector<->list list-reverse-iso) '(3 2 1))
  ]
}

@section{Library Isomorphisms}

@defthing[symbol<->string iso?]{
  Isomorphism between symbols and strings.
  @examples[
    #:eval op-eval
    (iso-modify symbol<->string 'foo string-upcase)
  ]
}

@defthing[vector<->list iso?]{
  Isomorphism between vectors and lists.
  @examples[
    #:eval op-eval
    (iso-modify vector<->list #(1 2 3) reverse)
  ]
}

@defthing[list-reverse-iso iso?]{
  Isomorphism between lists and reversed lists. Useful for folding over a traversal
  in reverse.
  @examples[
    #:eval op-eval
    (iso-forward list-reverse-iso (list 1 2 3))
    (iso-backward list-reverse-iso (list 3 2 1))
    (traversal-foldl (optic-compose vector<->list list-reverse-iso list-traversal)
                     #("a" "b" "c")
                     string-append
                     "")
  ]
}

@defthing[identity-iso iso?]{
  Isomorphism between values and themselves. The identity of @racket[optic-compose].
  @examples[
    #:eval op-eval
    (iso-forward identity-iso 1)
    (iso-backward identity-iso 1)
  ]
}

@section{Isomorphism Generic Interface}

@defidform[gen:iso]{
  A generic interface for isomorphisms.
  @examples[
    #:eval op-eval
    (struct make-iso [forward backward]
      #:methods gen:iso
      [(define (iso-forward iso target) ((make-iso-forward iso) target))
       (define (iso-backward iso focus) ((make-iso-backward iso) focus))])
  ]
}
