#lang scribble/manual

@require[scribble/example @for-label[(except-in racket set) ocular-patdown/optics/lens]]
@(define op-eval (make-base-eval))
@examples[#:hidden #:eval op-eval (require (except-in racket set) ocular-patdown)]

@title{Lenses}

@defmodule[ocular-patdown/optics/lens]

This module's bindings are also provided by @racketmodname[ocular-patdown/optics].

A @deftech{lens} is a type of @tech{optic} that has a single @tech{focus}. Lenses can be used to focus on a field of a struct, the @racket[car] of a pair, etc.

@examples[
  #:eval op-eval
  (define pair (cons 1 2))
  (lens-get car-lens pair)
  (lens-set car-lens pair #t)
  (struct posn [x y] #:transparent)
  (define posn1 (posn 1 2))
  (define posn-x-lens (struct-lens posn x))
  (lens-get posn-x-lens posn1)
  (lens-set posn-x-lens posn1 3)
]

@defproc[(lens? [val any/c]) boolean?]{
  A predicate which recognizes lenses. To be more precise, it recognizes implementers of @racket[gen:lens].
  @examples[
    #:eval op-eval
    (lens? car-lens)
    (lens? list-traversal)
    (lens? identity-iso)
  ]
}

@defproc[(make-lens [getter (-> any/c any/c)] [setter (-> any/c any/c any/c)]) lens?]{
  Constructor for lenses. @racket[getter] should extract the focus from the target.
  @racket[setter] should take in the target and the new value for the focus and return a copy of the target with the updated focus.
  @examples[
    #:eval op-eval
    (define my-car-lens (make-lens car (lambda (pair val) (cons val (cdr pair)))))
    (lens? my-car-lens)
    (lens-get my-car-lens (cons 1 2))
    (lens-set my-car-lens (cons 1 2) #t)
  ]
  There are a few laws lenses should obey:
  @itemize[
    @item{
      Getting the focus after setting the focus returns the new focus.
      @racketblock[
        (equal? (lens-get lens (lens-set lens target focus)) focus)
      ]
    }
    @item{
      Setting the focus using the current focus leaves the target unchanged.
      @racketblock[(equal? (lens-set lens target (lens-get lens target)) target)]
    }
    @item{
      Setting the focus twice is the same as setting it once with the second value.
      @racketblock[(equal? (lens-set lens (lens-set lens target focus1) focus2) (lens-set lens target focus2))]
    }
  ]
  These laws should be obeyed for some reasonable definition of equality. If these laws are not obeyed, you may experience unexpected behavior.

  Lenses should also be pure. In other words, lens operations should not mutate the target or the focus, or have any other side effects. For setters,
  it is recommended to create an updated copy of the original target rather than mutating it.
}

@defproc[(lens-get [lens lens?] [target any/c]) any/c]{
  Gets the focus from @racket[target] under @racket[lens].
  @examples[
    #:eval op-eval
    (lens-get car-lens (cons 1 2))
    (lens-get posn-x-lens (posn 3 4))
  ]
}

@defproc[(lens-set [lens lens?] [target any/c] [focus any/c]) any/c]{
  Returns an updated @racket[target] with @racket[focus] as the new focus under @racket[lens].
  @examples[
    #:eval op-eval
    (lens-set car-lens (cons 1 2) #t)
    (lens-set posn-x-lens (posn 3 4) 9)
  ]
}

@defproc[(lens-modify [lens lens?] [target any/c] [proc (-> any/c any/c)]) any/c]{
  Returns an updated @racket[target] with @racket[proc] applied to the focus under @racket[lens].
  @examples[
    #:eval op-eval
    (lens-modify car-lens (cons 1 2) sub1)
    (lens-modify posn-x-lens (posn 3 4) -)
  ]
}

@defproc[(lens-compose [lens lens?] ...) lens?]{
  Compose lenses like @racket[optic-compose].
  @examples[
    #:eval op-eval
    (struct tree [val children] #:transparent)
    (define tree-first-child-lens (lens-compose (struct-lens tree children) car-lens))
    (lens-set tree-first-child-lens
               (tree 1 (list (tree 2 '()) (tree 3 '())))
               (tree #t '()))
    (define second-lens (lens-compose cdr-lens car-lens))
    (lens-set second-lens (list 1 2) #t)
    (define first-of-second-lens (lens-compose second-lens car-lens))
    (lens-set first-of-second-lens (list 1 (list 2 3)) #t)
  ]
}

@section{Library Lenses}

@defthing[identity-lens lens?]{
  A lens that focuses on the entire target. The identity of @racket[lens-compose].
  @examples[
    #:eval op-eval
    (lens-get identity-lens 1)
    (lens-set identity-lens 1 2)
    (lens-modify identity-lens 1 add1)
  ]
}

@defthing[car-lens lens?]{
  A lens that focuses on the @racket[car] of a pair.
  @examples[
    #:eval op-eval
    (lens-get car-lens (cons 1 2))
    (lens-set car-lens (cons 1 2) #t)
  ]
}

@defthing[cdr-lens lens?]{
  A lens that focuses on the @racket[cdr] of a pair.
  @examples[
    #:eval op-eval
    (lens-get cdr-lens (cons 1 2))
    (lens-set cdr-lens (cons 1 2) #t)
  ]
}

@defthing[caar-lens lens?]{
  A lens that focuses on the @racket[caar] of a pair.
}

@defthing[cadr-lens lens?]
@defthing[cdar-lens lens?]
@defthing[cddr-lens lens?]

@defform[
  (struct-lens struct-id field-id maybe-parent)
  #:grammar ([maybe-parent (code:line) (code:line #:parent parent-struct-id)])
]{
  A lens that focuses on a field of a struct. Supply @racket[#:parent] if the field is a field of a supertype.
  @examples[
    #:eval op-eval
    (struct posn [x y] #:transparent)
    (define posn-x-lens (struct-lens posn x))
    (lens-get posn-x-lens (posn 3 4))
    (lens-set posn-x-lens (posn 3 4) 9)
    (struct posn3 posn [z] #:transparent)
    (define posn3-z-lens (struct-lens posn3 z))
    (define posn3-x-lens (struct-lens posn3 x #:parent posn))
    (lens-set posn3-z-lens (posn3 1 2 3) 9)
    (lens-set posn3-x-lens (posn3 1 2 3) 9)
    (lens-set posn-x-lens (posn3 1 2 3) 9)
  ]
  If the supertype is not supplied for fields of supertypes, an update will yield an instance of the supertype.
}

@section{Generic Lens Interface}

@defidform[gen:lens]{
  A generic interface for lenses.
  @examples[
    #:eval op-eval
    (struct make-lens (getter setter)
      #:methods gen:lens
      [(define (lens-get lens target) ((make-lens-getter lens) target))
       (define (lens-set lens target focus)
         ((make-lens-setter lens) target focus))])
  ]
}
