#lang scribble/manual
@require[scribble/example @for-label[(except-in racket set) ocular-patdown]]
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

@defproc[(get [optic lens?]) any/c]{
  Gets the focus of @racket[optic] using @racket[current-update-target].

  @examples[
    #:eval op-eval
    (update (cons 1 2) [(cons a _) (get a)])
    (parameterize ([current-update-target (cons 1 2)])
      (get car-lens))
  ]
}

@defproc[(set [optic lens?] [value any/c]) any/c]{
  Sets the focus of @racket[optic] to @racket[value] using @racket[current-update-target]. Also mutates @racket[current-update-target] and returns its new value.

  @examples[
    #:eval op-eval
    (update (cons 1 2) [(cons a _) (set a #t)])
    (current-update-target (cons 1 2))
    (set car-lens -1)
    (current-update-target)
  ]
}

@defproc[(modify [optic optic?] [proc (-> any/c any/c)]) any/c]{
  Updates the focus of @racket[optic] by applying @racket[proc], using @racket[current-update-target]. Also mutates @racket[current-update-target] and returns its new value.

  @examples[
    #:eval op-eval
    (update (cons 1 2) [(cons a _) (modify a sub1)])
    (current-update-target (cons 1 2))
    (modify cdr-lens sqr)
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
    (update (list 1 2) [(list a b) (set a 4) (current-update-target)])
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

@section{Optics}

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

@subsection{Lenses}

@defmodule[ocolar-patdown/optics/lens]

The bindings of this library are also provided by @racketmodname[ocular-patdown/optics].

A @deftech{lens} is a type of optic that has a single focus. Lenses can be used to focus on a field of a struct, the @racket[car] of a pair, etc.

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

@subsubsection{Library Lenses}

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

@subsubsection{Generic Lens Interface}

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
