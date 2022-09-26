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
  (struct rect [top-left width height] #:transparent)
  (define rect1 (rect (posn 10 20) 16 9))
  (define rect-top-left-lens (struct-lens rect top-left))
  (define rect-x-lens (lens-compose rect-top-left-lens posn-x-lens))
  (lens-get rect-x-lens rect1)
  (lens-set rect-x-lens rect1 35)

  (struct tree [val children] #:transparent)
  (define tree1 (tree 1 (list (tree 2 (list (tree 3 '()))) (tree 4 '()))))
  (define tree-children-lens (struct-lens tree children))
  (define tree-value-lens (struct-lens tree val))
  (define tree-first-value-lens (lens-compose tree-children-lens car-lens tree-value-lens))
  (lens-get tree-first-value-lens tree1)
  (lens-set tree-first-value-lens tree1 20)
]

When we compose lenses, the focus of the first is used as the target of the second. Using lens composition, we can perform very deep accesses
and modifications.

@section{Traversals}

Traversals are like lenses, except they can have zero or multiple @tech[#:key "focus"]{foci}. Traversals are useful for focusing on all the elements
of a collection. A traversal is like a first class @racket[map] and a @racket[foldl]{fold}.

@examples[
  #:eval op-eval
  (traversal-modify list-traversal '(1 2 3) add1)
  (traversal-modify rose-traversal '((1 2) (3 ((4)))) add1)
  (traversal-foldl rose-traversal '((1 2) (3 ((4)))) + 0)
  (traversal->list rose-traversal '((1 2) (3 ((4)))))
]

All lenses are traversals, but not all traversals are lenses. Lenses are just traversals that happen to have exactly
one target.

@examples[
  #:eval op-eval
  (traversal? car-lens)
  (traversal-modify car-lens '(1 2) number->string)
  (lens? list-traversal)
]

Like lenses, traversals can be composed. Each focus of the first traversal becomes the target for the second traversal.
All the inner foci of all the outer targets are foci of the composition.

@examples[
  #:eval op-eval
  (define lol-traversal (traversal-compose list-traversal list-traversal))
  (traversal-modify lol-traversal '((1 2 3) (4) ()) add1)
]

Where things really get interesting is when we compose lenses and traversals.

@examples[
  #:eval op-eval
  (define lop-x-traversal (traversal-compose list-traversal posn-x-lens))
  (traversal? lop-x-traversal)
  (traversal-modify lop-x-traversal (list (posn 10 20) (posn 30 40)) sqr)
  (define tree-child-value-traversal (traversal-compose tree-children-lens list-traversal tree-value-lens))
  (traversal? tree-child-value-traversal)
  (traversal-modify tree-child-value-traversal tree1 number->string)
]

When we compose a traversal with a lens, we get a traversal. Since lenses are traversals that happen to have a single target,
they work just fine with traversal composition. This, along with adding other optics into the mix, can be used to express a wide
variety of computations. Composing simple optics in this way allows you to focus on pretty much any values you'd want to in a target.

If you're writing an interpreter, you can create a traversal that focuses on the free variables of an expression. Then, you can use that
traversal to perform operations like substitution, renaming, getting a set of free variables, etc.

@examples[
  #:eval op-eval
(define free-var-traversal
  (make-traversal
   (lambda (proc expr)
     (let loop ([expr expr] [bound (set)])
       (match expr
         [(? symbol? x) (if (set-member? bound x) x (proc x))]
         [`(lambda (,(? symbol? x)) ,body) `(lambda (,x) ,(loop body (set-add bound x)))]
         [(list rator rand) (list (loop rator bound) (loop rand bound))])))
   (lambda (proc init expr)
     (let loop ([expr expr] [bound (set)] [acc init])
       (match expr
         [(? symbol? x) (if (set-member? bound x) acc (proc acc x))]
         [`(lambda (,(? symbol? x)) ,body) (loop body (set-add bound x) acc)]
         [(list rator rand) (loop rand bound (loop rator bound acc))])))))
(define (free-vars-set expr) (traversal-foldl free-var-traversal expr set-add (set)))
(free-vars-set '(f (lambda (x) (g x))))
(define (subst expr var replacement) (traversal-modify free-var-traversal expr (lambda (x) (if (eq? var x) replacement x))))
(define (eval expr)
  (match expr
    [(? symbol? x) (error 'eval "unbound var ~a" x)]
    [`(lambda (,(? symbol? x)) ,body) expr]
    [(list rator rand)
     (match (eval rator)
       [`(lambda (,(? symbol? x)) ,body) (eval (subst body x (eval rand)))]
         [expr (error 'eval "cannot apply non-function ~a" expr)])]))
(eval '(((lambda (x) (lambda (y) x)) (lambda (a) a)) (lambda (b) b)))
]
