#lang scribble/manual
@require[scribble/example @for-label[(except-in racket set) ocular-patdown ocular-patdown/optics]]
@(define op-eval (make-base-eval))
@examples[#:hidden #:eval op-eval (require (except-in racket set) ocular-patdown ocular-patdown/optics)]

@title{ocular-patdown}
@author{Mike Delmonaco}

@defmodule[ocular-patdown]

@;TODO link pat in clause to its racketgrammar
@;TODO figure out how to get stuff like 'and' to link to the 'and' pattern, not the one from racket/base.

@defform[
(update target-expr clause ...)
#:grammar
([clause [pat body ...+]])
]

Like @racket[match], but can be used for immutably updating values.

@examples[
#:eval op-eval
(update (list 1 2 3) [(list a b c) (set a #t) (modify b -)])
(struct posn [x y] #:transparent)
(update (posn 1 2) [(struct-field posn x x-value) (set x-value 3)])
(update (list (posn 1 2) (posn 3 4))
  [(list (struct-field posn x x-value) p)
   (modify x-value -)
   (set p (posn 5 6))])
(update (list (posn 1 2) (posn 3 4) (posn 5 6))
  [(list-of (struct-field posn x x-value)) (modify x-value -)])
(update (list 1 2 3)
  [(list a b) (error "boom")]
  [(list a b c) (set c 4)])
]

You can also use it to get values.

@examples[
#:eval op-eval
(update (list 1 2) [(list a b) (get a)])
(update (list 1 2) [(list a b) (set a (+ 4 (get b)))])
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
    (update (list 1 2) [(and a (list b c)) (set b (get a))])
]
}



@defform[(cons car-pat cdr-pat)]{

Matches pairs. @racket[car-pat] gets matched on the @racket[car], and @racket[cdr-pat] gets matched on the @racket[cdr].

@examples[
    #:eval op-eval
    (update (cons 1 2) [(cons a b) (set a 3)])
    (update (list #t #f) [(cons a (cons b _)) (set b 'true)])
]
}

@defform[(list pat ...)]{

Matches a list with as many elements as @racket[pat]s. Matches each element against its corresponding pattern.

@examples[
#:eval op-eval
(update (list 1 2) [(list a b) (modify b -)])
]
}

@defform[(list-of pat)]{

@;TODO mention traversals
Matches a list. Matches each element against @racket[pat], but optics bounds by @racket[pat] focus on all elements, not just one element.
It sort of "maps" @racket[pat] over the elements of the list. It is also similar to @racket[pat ...] in @racket[match] in that variables of @racket[pat] get bound to lists.

@examples[
#:eval op-eval
(update (list 1 2 3 4) [(list-of n) (modify n sqr)])
(update '((1 2 3) (4 5 6) () (7)) [(list-of (list-of n)) (modify n sqr)])
(update (list (posn 1 2) (posn 3 4)) [(list-of (struct-field posn x)) (modify x -)])
]
}

@defform*[
((struct-field struct-name field-name pat)
 (struct-field struct-name field-name))
]{

Matches a struct which is an instance of the struct type named @racket[struct-id]. Focuses on the field @racket[field-name] and matches @racket[pat] against its value.

The second form is shorthand for @racket[(struct-field struct-name field-name field-name)].

Cannot be used on fields from a struct's super type.

@examples[
#:eval op-eval
(update (posn 1 2) [(struct-field posn x x-value) (set x-value 3)])
(update (posn 1 2) [(struct-field posn x) (set x 3)])
]

@examples[
#:eval op-eval
#:label "Be careful with struct subtypes:"
(struct posn3 posn [z] #:transparent)
(update (posn3 3 4 5) [(struct-field posn3 z) (set z 9)])
(update (posn3 3 4 5) [(struct-field posn x) (set x 9)])
]

Naively trying to use a super type's struct field to perform an update on an instance of the subtype will yield an instance of the super type.
}

@defform[
(iso target? forward backward pat)
#:contracts ([target? (-> any/c boolean?)] [forward (-> A B)] [backward (-> B A)])
]{

Matches a value that satisfies @racket[target?]. @racket[target?] should recognize values of type @racket[A]. @racket[forward] should convert values from @racket[A] to @racket[B], and @racket[backward] the inverse.
This matches values of type @racket[A] and matches @racket[pat] against its equivalent value of type @racket[B] according to @racket[forward]. Useful for treating a value of one type as another.

@examples[
#:eval op-eval
(update 'foo [(iso symbol? symbol->string string->symbol str) (modify str string-upcase)])
]

@racket[backward] must be the inverse of @racket[forward]. Specifically, for any value @racket[v]

@racketblock[
(when (target? v) (equal? v (backward (forward v))))
]

If @racket[(target? v)] is true, @racket[v] must be equal to @racket[(backward (forward v))] for some reasonable definition of equality.
}

@defform[
(optic target? optic-expr pat)
#:contracts ([target? (-> any/c boolean?)] [optic-expr optic?])
]{

Matches a value that satisfies @racket[target?]. Matches @racket[pat] against the focus or foci of @racket[optic-expr], where @racket[optic-expr] is an @tech{optic}.
This is useful for using optics as patterns and defining new patterns using optics and @racket[define-pattern-syntax]. In fact, most of the standard patterns were defined
this way.

@examples[
#:eval op-eval
(update (cons 1 2) [(optic cons? car-lens a) (modify a sub1)])
]
}

@section{Getter and Updater Utilities}

@racket[get], @racket[set], and @racket[modify] are just ordinary procedures that operate on optics. The only thing that is special about them is that they know about the current target of an @racket[update].

@defproc[(get [optic optic?]) any/c]{
  Gets the focus of @racket[optic] using @racket[current-update-target].

  @examples[
    #:eval op-eval
    (parameterize ([current-update-target (cons 1 2)])
      (get car-lens))
  ]
}

@defproc[(set [optic optic?] [value any/c]) any/c]{
  Sets the focus of @racket[optic] to @racket[value] using @racket[current-update-target]. Also mutates @racket[current-update-target] and returns its new value.

  @examples[
    #:eval op-eval
    (current-update-target (cons 1 2))
    (set car-lens -1)
    (current-update-target)
  ]
}

@defproc[(modify [optic optic?] [proc (-> any/c any/c)]) any/c]{
  Updates the focus of @racket[optic] by applying @racket[proc], using @racket[current-update-target]. Also mutates @racket[current-update-target] and returns its new value.

  @examples[
    #:eval op-eval
    (current-update-target (cons 1 2))
    (modify cdr-lens sqr)
    (current-update-target)
  ]
}

@defproc[(fold [optic optic?] [proc (-> any/c any/c any/c)] [init any/c]) any/c]{
 Folds (@racket[foldl]) over the foci of @racket[optic] using @racket[current-update-target].

  @examples[
    #:eval op-eval
    (parameterize ([current-update-target (list 1 2 3)])
      (fold list-traversal cons '()))
  ]
}

@defparam[current-update-target target any/c]{
  A parameter that defines the current target of an @racket[update] expression.

  @examples[
    #:eval op-eval
    (update (list 1 2) [(list a b) (current-update-target)])
    (update (list 1 2) [(list a b) (set a 4) (current-update-target)])
  ]
}

@section{Extending Update}

@defform*[((define-pattern-syntax id transformer-expr)
           (define-pattern-syntax (id arg-id ...) body-expr ...+))
           #:contracts ([transformer-expr (-> syntax? syntax?)])]{
  Like @racket[define-syntax], but defines a macro for patterns in @racket[update]. Macro names are only visible within @racket[update] expressions, so they will not shadow names provided by Racket.

  The first form defines a macro named @racket[id] bound to @racket[transformer-expr]. The second form is shorthand for @racket[(define-pattern-syntax id (lambda (arg-id ...) (begin body-expr ...)))].

  @;{
  @examples[
    #:eval op-eval
    (define-pattern-syntax posn
      (syntax-rules ()
        [(posn x-pat y-pat)
         (and (struct-field posn x x-pat) (struct-field posn y y-pat))]))
    (update (posn 1 2) [(posn a b) (set a 4) (modify b -)])
  ]
  }
}
