#lang scribble/manual
@require[scribble/example @for-label[(except-in racket set) ocular-patdown/optics/lens ocular-patdown/optics/traversal ocular-patdown/optics/isomorphism ocular-patdown/optics ocular-patdown/update]]
@(define op-eval (make-base-eval))
@; from https://github.com/racket/racket/blob/8fae9072302dd7b9f48d13356c411cd0876da6db/pkgs/racket-doc/syntax/scribblings/parse/parse-common.rkt#L97C1-L109C66
@(define-syntax ref
  (syntax-rules ()
    [(ref id suffix ...)
     (elemref (list 'pattern-link (list 'id 'suffix ...))
              (racketkeywordfont (symbol->string 'id))
              (superscript (symbol->string 'suffix)) ...
              #:underline? #f)]))
@(define-syntax def
  (syntax-rules ()
    [(def id suffix ...)
     (elemtag (list 'pattern-link (list 'id 'suffix ...))
              (racket id)
              #|(superscript (symbol->string 'suffix)) ...|# )]))
@examples[#:hidden #:eval op-eval (require (except-in racket set) ocular-patdown)]

@title[#:tag "update-reference"]{Pattern-based Updating}

@defmodule[ocular-patdown/update]


@;TODO link pat in clause to its racketgrammar
@;TODO figure out how to get stuff like 'and' to link to the 'and' pattern, not the one from racket/base.

@defform[
(update target-expr clause ...)
#:grammar
([clause [pat body ...+]])
]

Like @racket[match] for performing immutable updates. Also useful for creating composed @tech{optics}.

@examples[#:eval op-eval
(update (list 1 2 3)
  [(list a b c)
   (set! a #t)
   (modify! b -)])
]

Patterns act like trees of optic compositions (see @racket[optic-compose]) with variables as leaves, binding composed optics. However, pattern-bound variables have special behavior when used in a clause body. Variable references retrieve the value of the @tech{focus} of its optic. The variable must be "single-valued" (correspond to a lens). Using @racket[set!] on a pattern-bound variable will update the current copy of the @tech{target} using the optic to set the focus to a new value (see @racket[optic-set!]). Note that we are only mutating a parameter tracking the current copy of the target, not the target itself. To retrieve a variable's optic itself, use @ref[optic b].

@examples[
#:eval op-eval
#:label "More examples"
(struct posn [x y] #:transparent)
(code:comment "get focus")
(update (posn 1 2)
  [(posn x)
   x])
(code:comment "set focus")
(update (posn 1 2)
  [(posn x)
   (set! x 3)])
(code:comment "no mutation of the original target")
(define p (posn 1 2))
(update p
  [(posn x)
   (set! x 3)])
p
(code:comment "multiple independent updates")
(update (list (posn 1 2) (posn 3 4))
  [(list (posn x) p)
   (modify! x -)
   (set! p (posn 5 6))])
(code:comment "'multi-valued' update")
(update (list (posn 1 2) (posn 3 4) (posn 5 6))
  [(list-of (posn x))
   (modify! x -)])
(code:comment "cannot use references or set! on a 'multi-valued' variable")
(eval:error
 (update (list 1 2 3)
   [(list-of x)
    x]))
(eval:error
 (update (list 1 2 3)
   [(list-of x)
    (set! x 0)]))
(code:comment "multiple clauses")
(update (list 1 2 3)
  [(list a b)
  (error "boom")]
  [(list a b c)
   (set! c 4)])
(code:comment "get and set")
(update (list 1 2)
  [(list a b)
   (set! a (+ 4 b))])
]

@section{Patterns}

The grammar for patterns is as follows:

@racketgrammar[
#:literals (_ optic and cons list list-of struct-field iso)
pat
_
id
(and pat ...)
(cons pat pat)
(list pat ...)
(list-of pat)
(struct-field struct-id id pat)
(struct-field struct-id id)
(struct-id field-spec ...)
(iso expr expr expr pat)
(optic expr expr pat)
]

@racketgrammar[
field-spec
[field-id pat]
field-id
]

More details on the different patterns:

@defidform[_]{

    Matches any value and binds nothing.

    @examples[
        #:eval op-eval
        (update 1 [_ 2])
    ]
}

@defidform[id]{

    Matches any value and binds the @racket[id] to the @tech{optic} focusing on the value(s) specified by this pattern.

    @examples[
        #:eval op-eval
        (update 1 [a a])
    ]
}



@defform[(and pat ...)]{

Match all the patterns on the same value.

@examples[
    #:eval op-eval
    (update (list 1 2)
      [(and a (list b c))
       (set! b a)])
]
}



@defform[(cons car-pat cdr-pat)]{

Matches pairs. @racket[car-pat] gets matched on the @racket[car] (composes the @racket[car-lens] optic), and @racket[cdr-pat] gets matched on the @racket[cdr] (composes the @racket[cdr-lens] optic).

@examples[
    #:eval op-eval
    (update (cons 1 2)
      [(cons a b)
       (set! a 3)])
    (update (list #t #f)
      [(cons a (cons b _))
       (set! b 'true)])
]
}

@defform[(list pat ...)]{

Matches a list with as many elements as @racket[pat]s. Matches each element against its corresponding pattern. Composes @racket[car-lens] and @racket[cdr-lens] appropriately.

@examples[
#:eval op-eval
(update (list 1 2)
  [(list a b)
   (modify! b -)])
]
}

The last pattern can be ellipsized, which will give it the behavior of @racket[list-of] on the tail of the list.

@examples[
#:eval op-eval
(update (list 1 2 3 4 5)
  [(list a b ...)
   (modify! b add1)])
]

@defform[(list-of pat)]{

Matches a list. Matches each element against @racket[pat], but optics bounds by @racket[pat] focus on @emph{all} elements, not just one element. Composes @racket[list-traversal].

@examples[
#:eval op-eval
(update (list 1 2 3 4)
  [(list-of n)
   (modify! n sqr)])
(update '((1 2 3) (4 5 6) () (7))
  [(list-of (list-of n))
   (modify! n sqr)])
(update (list (posn 1 2) (posn 3 4))
  [(list-of (posn x))
   (modify! x -)])
]
}

@defform*[
((struct-field struct-name field-name pat)
 (struct-field struct-name field-name))
]{

Matches a struct which is an instance of the struct type named @racket[struct-id]. Focuses on the field @racket[field-name] and matches @racket[pat] against its value. Composes a @racket[struct-lens].

The second form is shorthand for @racket[(struct-field struct-name field-name field-name)].

Cannot be used on fields from a struct's super type.

@examples[
#:eval op-eval
(update (posn 1 2)
  [(struct-field posn x x-value)
   (set! x-value 3)])
(update (posn 1 2)
  [(struct-field posn x)
   (set! x 3)])
]

@examples[
#:eval op-eval
#:label "Be careful with struct subtypes:"
(struct posn3 posn [z] #:transparent)
(update (posn3 3 4 5)
  [(struct-field posn3 z)
   (set! z 9)])
(update (posn3 3 4 5)
  [(struct-field posn x)
   (set! x 9)])
]

Naively trying to use a super type's struct field to perform an update on an instance of the subtype will yield an instance of the super type.
}

@defform[
(struct-id field-spec ...)
]

A wrapper around @racket[struct-field] for syntactic convenience. The order of fields doesn't matter and it is not necessary to supply all fields.

@examples[
#:eval op-eval
(update (posn 1 2)
  [(posn y [x a])
   (set! y 3)
   (set! a 4)])
]

@defform[
(iso target? forward backward pat)
#:contracts ([target? (-> any/c boolean?)] [forward (-> any/c any/c)] [backward (-> any/c any/c)])
]{

Matches a value that satisfies @racket[target?]. Matches @racket[pat] against the result of @racket[forward] applied to the target. Composes the isomorphism @racket[(make-iso forward backward)].
As such, @racket[forward] and @racket[backward] should be inverse functions of each other.

Useful for treating values of one type as values of another, equivalent type.

@examples[
#:eval op-eval
(update 'foo
  [(iso symbol? symbol->string string->symbol str)
   (modify! str string-upcase)])
]
}

@defidform[optic]{
One of
@itemlist[
@item{@ref[optic p] for use inside of a pattern.}
@item{@ref[optic b] for use inside of the body of a clause.}
]
}

@specform[
(@#,def[optic p] target? optic-expr pat)
#:contracts ([target? (-> any/c boolean?)] [optic-expr optic?])
]{

Matches a value that satisfies @racket[target?]. Matches @racket[pat] against the @tech{focus} or foci of @racket[optic-expr], where @racket[optic-expr] is an @tech{optic}. Composes @racket[optic-expr].

This is useful for using optics as patterns and defining new patterns using optics and @racket[define-update-syntax]. In fact, most of the standard patterns are defined this way.

@examples[
#:eval op-eval
(update (cons 1 2)
  [(optic cons? car-lens a)
   (set! a 0)])
]
}

@section{Getter and Updater Utilities}

@defparam[current-update-target target any/c]{
  A parameter that is equal to the current target of an @racket[update] expression.
  Used by forms like @racket[set!] and @racket[modify!].

  @examples[
    #:eval op-eval
    (update (list 1 2)
      [(list a b)
       (current-update-target)])
    (update (list 1 2)
      [(list a b)
       (set! a 4)
       (current-update-target)])
  ]
}

@specform[
(@#,def[optic b] optic-id)
]{
  Gets the optic corresponding to @racket[optic-id].

  @examples[
    #:eval op-eval
    (define op
      (update (cons 1 2)
        [(cons a _)
         (optic a)]))
    op
    (lens-get op (cons 1 2))
  ]
}

@defproc[(optic-set! [op lens?] [value any/c]) any/c]{
  Sets the focus of @racket[op] to @racket[value] using @racket[current-update-target]. Also mutates @racket[current-update-target] and returns its new value. Note that @racket[op] must be a lens, not a @tech{traversal} like from @racket[list-of].

  Using @racket[set!] on a variable bound by @racket[update] will use @racket[optic-set!], so you can just use @racket[set!] instead.

  @examples[
    #:eval op-eval
    (update (cons 1 2)
      [(cons a _)
       (optic-set! (optic a) #t)])
    (update (cons 1 2)
      [(cons a _)
       (set! a #t)])
    (current-update-target (cons 1 2))
    (optic-set! car-lens -1)
    (current-update-target)
  ]
}

@defform[(modify! optic-var proc)]{
  Updates the focus of the optic corresponding to @racket[optic-var] by applying @racket[proc], using @racket[current-update-target]. Also mutates @racket[current-update-target] and returns its new value. @racket[optic-var] need not refer to a lens. It can also refer to a traversal.

  @examples[
    #:eval op-eval
    (update (list 1 2 3 4)
      [(list-of n)
       (modify! n sqr)])
  ]
}

@defform[(fold optic-var proc init)]{
 Folds (like @racket[foldl]) over the foci of @racket[optic-var] using @racket[current-update-target].

  @examples[
    #:eval op-eval
    (update (list 1 2 3)
      [(list-of a)
       (fold a + 0)])
    (update (list 1 2 3)
      [(list-of a)
       (fold a cons (list))])
  ]
}

@section{Extending Update}

@defform*[((define-update-syntax id transformer-expr))
           #:contracts ([transformer-expr (-> syntax? syntax?)])]{
  Like @racket[define-syntax], but defines a macro for patterns in @racket[update]. Macro names are only visible within @racket[update] expressions, so they will not shadow names provided by Racket.

  Defines a pattern macro named @racket[id] bound to @racket[transformer-expr]. @racket[id] is defined in a separate @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{binding space}, so it is safe to shadow names that are already defined by Racket. This macro is only usable in the pattern language of @racket[update].

  @examples[
    #:eval op-eval
    (define-update-syntax list*
      (syntax-rules ()
        [(list* p) p]
        [(list* p0 p ...) (cons p0 (list* p ...))]))
    (update (list 1 2 3 4 5 6 7)
      [(list* a b c (list-of n))
       (set! b 0)
       (modify! n sqr)])
  ]
}
