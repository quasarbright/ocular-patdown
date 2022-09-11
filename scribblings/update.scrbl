#lang scribble/manual
@require[scribble/example @for-label[(except-in racket set) ocular-patdown/optics/lens ocular-patdown/optics/traversal ocular-patdown/optics/isomorphism ocular-patdown/optics ocular-patdown/update]]
@(define op-eval (make-base-eval))
@examples[#:hidden #:eval op-eval (require (except-in racket set) ocular-patdown)]

@title{Pattern-based Updating}

@defmodule[ocular-patdown/update]


@;TODO link pat in clause to its racketgrammar
@;TODO figure out how to get stuff like 'and' to link to the 'and' pattern, not the one from racket/base.

@defform[
(update target-expr clause ...)
#:grammar
([clause [pat body ...+]])
]

Like @racket[match], but binds @tech{optic}s. Patterns can be thought of as trees of optic compositions (see @racket[optic-compose]) with variables as leaves, binding composed optics.

@examples[
 #:eval op-eval
 (update (list 1 2 3) [(list a b c) b])
]

Use @racket[current-update-target] to use the optics on the target of the update.

@examples[
  #:eval op-eval
 (update (list 1 2 3) [(list a b c) (optic-get b (current-update-target))])
 (update (list 1 2 3) [(list a b c) (optic-set b (current-update-target) #t)])
]

This is cumbersome, especially when you want to perform multiple updates in sequence.
As such, the library provides helpers like @racket[get] and @racket[optic-set!] which thread the parameter
implicitly. Operations like @racket[optic-set!] have the side effect of mutating the value of the parameter.

@examples[
#:eval op-eval
(update (list 1 2 3) [(list a b c) (optic-set! a #t) (modify! b -)])
(struct posn [x y] #:transparent)
(update (posn 1 2) [(struct-field posn x x-value) (optic-set! x-value 3)])
(update (list (posn 1 2) (posn 3 4))
  [(list (struct-field posn x x-value) p)
   (modify! x-value -)
   (optic-set! p (posn 5 6))])
(update (list (posn 1 2) (posn 3 4) (posn 5 6))
  [(list-of (struct-field posn x x-value)) (modify! x-value -)])
(update (list 1 2 3)
  [(list a b) (error "boom")]
  [(list a b c) (optic-set! c 4)])
(update (list 1 2) [(list a b) (get a)])
(update (list 1 2) [(list a b) (optic-set! a (+ 4 (get b)))])
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
(iso expr expr expr pat)
(optic expr expr pat)
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
        (update 1 [a (get a)])
    ]
}



@defform[(and pat ...)]{

Match all the patterns.

@examples[
    #:eval op-eval
    (update (list 1 2) [(and a (list b c)) (optic-set! b (get a))])
]
}



@defform[(cons car-pat cdr-pat)]{

Matches pairs. @racket[car-pat] gets matched on the @racket[car] (composes the @racket[car-lens] optic), and @racket[cdr-pat] gets matched on the @racket[cdr] (composes the @racket[cdr-lens] optic).

@examples[
    #:eval op-eval
    (update (cons 1 2) [(cons a b) (optic-set! a 3)])
    (update (list #t #f) [(cons a (cons b _)) (optic-set! b 'true)])
]
}

@defform[(list pat ...)]{

Matches a list with as many elements as @racket[pat]s. Matches each element against its corresponding pattern. Composes @racket[car-lens] and @racket[cdr-lens] appropriately.

@examples[
#:eval op-eval
(update (list 1 2) [(list a b) (modify! b -)])
]
}

@defform[(list-of pat)]{

Matches a list. Matches each element against @racket[pat], but optics bounds by @racket[pat] focus on all elements, not just one element. Composes @racket[list-traversal].

@examples[
#:eval op-eval
(update (list 1 2 3 4) [(list-of n) (modify! n sqr)])
(update '((1 2 3) (4 5 6) () (7)) [(list-of (list-of n)) (modify! n sqr)])
(update (list (posn 1 2) (posn 3 4)) [(list-of (struct-field posn x)) (modify! x -)])
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
(update (posn 1 2) [(struct-field posn x x-value) (optic-set! x-value 3)])
(update (posn 1 2) [(struct-field posn x) (optic-set! x 3)])
]

@examples[
#:eval op-eval
#:label "Be careful with struct subtypes:"
(struct posn3 posn [z] #:transparent)
(update (posn3 3 4 5) [(struct-field posn3 z) (optic-set! z 9)])
(update (posn3 3 4 5) [(struct-field posn x) (optic-set! x 9)])
]

Naively trying to use a super type's struct field to perform an update on an instance of the subtype will yield an instance of the super type.
}

@defform[
(iso target? forward backward pat)
#:contracts ([target? (-> any/c boolean?)] [forward (-> any/c any/c)] [backward (-> any/c any/c)])
]{

Matches a value that satisfies @racket[target?]. Matches @racket[pat] against the result of @racket[forward] applied to the target. Composes the isomorphism @racket[(make-iso forward backward)].
As such, @racket[forward] and @racket[backward] should be inverse functions of each other. Useful for treating values of one type as values of another, equivalent type.

@examples[
#:eval op-eval
(update 'foo [(iso symbol? symbol->string string->symbol str) (modify! str string-upcase)])
]
}

@defform[
(optic target? optic-expr pat)
#:contracts ([target? (-> any/c boolean?)] [optic-expr optic?])
]{

Matches a value that satisfies @racket[target?]. Matches @racket[pat] against the @tech{focus} or foci of @racket[optic-expr], where @racket[optic-expr] is an @tech{optic}. Composes @racket[optic-expr].
This is useful for using optics as patterns and defining new patterns using optics and @racket[define-pattern-syntax]. In fact, most of the standard patterns are defined
this way.

@examples[
#:eval op-eval
(update (cons 1 2) [(optic cons? car-lens a) (modify! a sub1)])
]
}

@section{Getter and Updater Utilities}

@racket[get], @racket[optic-set!], and @racket[modify!] are just ordinary procedures that operate on optics. The only thing that is special about them is that they know about the current target of an @racket[update].

@defproc[(get [optic lens?]) any/c]{
  Gets the focus of @racket[optic] using @racket[current-update-target].

  @examples[
    #:eval op-eval
    (update (cons 1 2) [(cons a _) (get a)])
    (parameterize ([current-update-target (cons 1 2)])
      (get car-lens))
  ]
}

@defproc[(optic-set! [optic lens?] [value any/c]) any/c]{
  Sets the focus of @racket[optic] to @racket[value] using @racket[current-update-target]. Also mutates @racket[current-update-target] and returns its new value.

  @examples[
    #:eval op-eval
    (update (cons 1 2) [(cons a _) (optic-set! a #t)])
    (current-update-target (cons 1 2))
    (optic-set! car-lens -1)
    (current-update-target)
  ]
}

@defproc[(modify! [optic optic?] [proc (-> any/c any/c)]) any/c]{
  Updates the focus of @racket[optic] by applying @racket[proc], using @racket[current-update-target]. Also mutates @racket[current-update-target] and returns its new value.

  @examples[
    #:eval op-eval
    (update (cons 1 2) [(cons a _) (modify! a sub1)])
    (current-update-target (cons 1 2))
    (modify! cdr-lens sqr)
    (current-update-target)
  ]
}

@defproc[(fold [optic optic?] [proc (-> any/c any/c any/c)] [init any/c]) any/c]{
 Folds (@racket[foldl]) over the foci of @racket[optic] using @racket[current-update-target].

  @examples[
    #:eval op-eval
    (update (list 1 2 3) [(list-of a) (fold a + 0)])
    (parameterize ([current-update-target (list 1 2 3)])
      (fold list-traversal cons '()))
  ]
}

@defparam[current-update-target target any/c]{
  A parameter that defines the current target of an @racket[update] expression.

  @examples[
    #:eval op-eval
    (update (list 1 2) [(list a b) (current-update-target)])
    (update (list 1 2) [(list a b) (optic-set! a 4) (current-update-target)])
  ]
}

@section{Extending Update}

@defform*[((define-pattern-syntax id transformer-expr)
           (define-pattern-syntax (id arg-id ...) body-expr ...+))
           #:contracts ([transformer-expr (-> syntax? syntax?)])]{
  Like @racket[define-syntax], but defines a macro for patterns in @racket[update]. Macro names are only visible within @racket[update] expressions, so they will not shadow names provided by Racket.

  The first form defines a macro named @racket[id] bound to @racket[transformer-expr]. The second form is shorthand for
  @racketblock[
    (define-pattern-syntax id
      (lambda (arg-id ...)
        (begin body-expr ...)))
  ]

  @examples[
    #:eval op-eval
    (define-pattern-syntax posn
      (syntax-rules ()
        [(posn x-pat y-pat)
         (and (struct-field posn x x-pat) (struct-field posn y y-pat))]))
    (update (posn 1 2) [(posn a b) (optic-set! a 4) (modify! b -)])
  ]
}
