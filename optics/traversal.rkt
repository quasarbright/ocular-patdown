#lang racket

(module+ test (require rackunit))

(provide
 ; generic interface for traversal
 gen:traversal
 ; traversal predicate
 traversal?
 #;(-> (-> (-> focus/c focus/c) target/c target/c)
       (-> (-> focus/c any/c any/c) any/c target/c any/c)
       traversal?)
 ; Create a traversal from a mapping function and a function that generates a sequence of all foci
 ; The order of arguments goes against the convention of the lens library, but matches the convention of
 ; map to make the creation of traversals easier.
 ; example:
 #;(define list-map-traversal (make-traversal map foldl))
 make-traversal
 ; traversal that focuses on all elements of a list.
 list-traversal
 ; traversal that focuses on all elements of a vector.
 vector-traversal
 ; traversal that focuses on the leaves of a rose tree.
 ; A rose-tree is a non-list or a (listof rose-tree)
 rose-traversal
 ; A traversal that focuses on the target if it is not #f, and has no focus otherwise.
 maybe-traversal
 ; A lazy traversal that isn't evaluated until it is used. It is evaluated at most once.
 ; Useful for creating recursive traversals.
 lazy-traversal
 #;(-> (-> target/c traversal?) traversal?)
 ; a traversal that depends on its target. Useful for creating conditional traversals like traversal-cond.
 dependent-traversal
 ; conditional traversal like if
 traversal-if
 ; conditional traversal like cond
 traversal-cond
 #;(traversal? ... -> traversal?)
 ; A traversal that focuses on the foci of each traversal
 ; traversals must have non-overlapping foci for all targets
 traversal-append
 #;(predicate? traversal? -> traversal?)
 ; A traversal that filters the foci of the given traversal.
 ; Be very careful. It is basically impossible to make this generally lawful.
 traversal-filter
 #;(-> traversal? target/c (-> focus/c focus/c) target/c)
 ; apply a procedure to each focus and return the updated target
 traversal-map
 #;(-> traversal? target/c (-> focus/c any/c any/c) any/c any/c)
 ; foldl over the target's foci. Unlike list foldl, the target comes first to conform to the library's convention
 traversal-foldl
 ; compose traversals. deepest/innermost traversal last.
 traversal-compose
 #;(-> traversal? target? (listof focus?))
 ; get a list of all foci
 traversal->list)



(require racket/generic racket/generator (for-syntax syntax/parse))



(define-generics traversal
  (traversal-map traversal target proc)
  (traversal-foldl traversal target proc init))

(struct make-traversal (map foldl)
  #:methods gen:traversal
  [(define (traversal-map t target proc)
     ((make-traversal-map t) proc target))
   (define (traversal-foldl t target proc init)
     ((make-traversal-foldl t) proc init target))])

; traversal that focuses on all elements of a list
(define list-traversal (make-traversal map foldl))

; traversal that focuses on all elements of a vector
(define vector-traversal (make-traversal vector-map
                                             (λ (proc init v) (for/fold ([acc init]) ([ele v]) (proc ele acc)))))

; traversal that focuses on the target itself
(define identity-traversal (make-traversal (λ (proc v) (proc v)) (λ (proc init v) (proc v init))))

; traversal with zero foci
(define empty-traversal (make-traversal (λ (proc v) v) (λ (proc init v) init)))

#;(-> (-> any/c any/c) rose? rose?)
; map over the leaves of a rose tree.
(define (rose-map proc rose)
  (if (list? rose)
      (map (λ (child) (rose-map proc child)) rose)
      (let ([result (proc rose)])
        (if (list? result)
            (error 'rose-traversal "cannot set a rose leaf to a list")
            result))))

#;(-> (-> any/c any/c any/c) any/c rose? any/c)
; foldl over the leaves of the rose tree.
(define (rose-foldl proc init rose)
  (for/fold ([acc init])
            ([element (rose->sequence rose)])
    (proc element acc)))

#;(-> (rose-of A) (sequence/c A))
; iterate the leaves of the rose tree.
(define (rose->sequence rose)
  (in-generator
   (let loop ([rose rose])
     (if (list? rose)
         (for ([child rose]) (loop child))
         (yield rose)))))

(module+ test
  (check-equal? (sequence->list (rose->sequence '((1 2) () (((((3)) 4))))))
                '(1 2 3 4)))

; traversal that focuses on the leaves of a rose-tree.
; A rose is a non-list or a (listof rose).
(define rose-traversal (make-traversal rose-map rose-foldl))

; traversal that focuses on the target if it is not #f. Has no focus otherwise.
(define maybe-traversal
      (make-traversal
        (lambda (proc val) (if val (proc val) val))
        (lambda (proc init val) (if val (proc val init) init))))

(module+ test
  (check-equal? (traversal-map list-traversal '(1 2 3) add1) '(2 3 4))
  (check-equal? (traversal-foldl list-traversal '(1 2 3) cons '()) '(3 2 1))
  (check-equal? (traversal-map vector-traversal #(1 2 3) add1) #(2 3 4))
  (check-equal? (traversal-foldl vector-traversal #(1 2 3) cons '()) '(3 2 1))
  (check-equal? (traversal-map identity-traversal 1 add1) 2)
  (check-equal? (traversal-foldl identity-traversal 1 cons '()) '(1))
  (check-equal? (traversal-map rose-traversal '((1) ((2 3) 4)) add1)
                '((2) ((3 4) 5)))
  (check-equal? (traversal->list rose-traversal '((1) ((2 3) 4))) '(1 2 3 4))
  (check-equal? (traversal-map maybe-traversal 1 add1) 2)
  (check-equal? (traversal-map maybe-traversal #f add1) #f)
  (check-equal? (traversal-foldl maybe-traversal 1 + 0) 1)
  (check-equal? (traversal-foldl maybe-traversal #f + 0) 0)
  (test-exn "cannot change number of targets"
            exn:fail?
            (thunk (traversal-map rose-traversal '((1 2)) (λ (v) (list v v))))))



#;(-> traversal? traversal? traversal?)
; compose two traversals
(define (traversal-compose2 outer-traversal inner-traversal)
  (make-traversal (λ (inner-proc outer-target)
                    (traversal-map outer-traversal
                                      outer-target
                                      (λ (inner-target)
                                        (traversal-map inner-traversal inner-target inner-proc))))
                  (λ (proc init outer-target)
                    (traversal-foldl outer-traversal outer-target
                                     (λ (inner-target outer-acc)
                                       (traversal-foldl inner-traversal inner-target proc outer-acc))
                                     init))))

(module+ test
  (define lov-traversal (traversal-compose2 list-traversal vector-traversal))
  (check-equal? (traversal-map lov-traversal '(#(1) #(2 3)) add1) '(#(2) #(3 4)))
  (check-equal? (traversal-foldl lov-traversal '(#(1) #(2 3)) cons '()) '(3 2 1)))

#;(-> traversal? ... traversal?)
; compose traversals
(define (traversal-compose . traversals)
  (foldr traversal-compose2 identity-traversal traversals))

(module+ test
  (check-equal? (traversal-map (traversal-compose list-traversal vector-traversal)
                                  '(#(1))
                                  add1)
                '(#(2))))

; traversals must have non-overlapping foci for all targets
(define (traversal-append . traversals)
  (foldr traversal-append2 empty-traversal traversals))

(define (traversal-append2 t1 t2)
  (make-traversal (λ (proc target) (traversal-map t2 (traversal-map t1 target proc) proc))
                  (λ (proc init target) (traversal-foldl t2 target proc (traversal-foldl t1 target proc init)))))

(module+ test
  ; folded in append order
  (check-equal? (traversal->list (traversal-append (traversal-filter even? list-traversal)
                                                   (traversal-filter odd? list-traversal))
                                 '(1 2 3 4))
                '(2 4 1 3))
  ; map preserves order
  (check-equal? (traversal-map (traversal-append (traversal-filter even? list-traversal)
                                                    (traversal-filter (λ (x) (= 1 x)) list-traversal))
                                  '(1 2 3 4)
                                  -)
                '(-1 -2 3 -4)))

; filters a traversal's foci
(define (traversal-filter pred traversal)
    (make-traversal (λ (proc target)
                      (traversal-map traversal
                                        target
                                        (λ (focus) (if (pred focus) (proc focus) focus))))
                    (λ (proc init target)
                      (traversal-foldl traversal
                                       target
                                       (λ (focus acc) (if (pred focus) (proc focus acc) acc))
                                       init))))

(module+ test
  (define even-list-traversal (traversal-filter even? list-traversal))
  (check-equal? (traversal->list even-list-traversal '(1 2 3 4)) '(2 4))
  (check-equal? (traversal-map even-list-traversal '(1 2 3 4) -) '(1 -2 3 -4)))

; chooses a traversal based on `(pred target)`.
; predicate must not depend on foci. To be more precise, no modification
; may cause the truthiness of `(pred target)` to change.
; then-traversal and else-traversal are wrapped in lazy-traversal.
(define-syntax-rule
  (traversal-if pred then-traversal else-traversal)
  (let ([pred-v pred])
    (traversal-if/strict pred (lazy-traversal then-traversal) (lazy-traversal else-traversal))))

(module+ test
  (define vector-or-list-traversal (traversal-if list? list-traversal vector-traversal))
  (check-equal? (traversal->list vector-or-list-traversal '(1 2 3)) '(1 2 3))
  (check-equal? (traversal->list vector-or-list-traversal #(1 2 3)) '(1 2 3))
  (check-equal? (traversal-map vector-or-list-traversal '(1 2 3) add1) '(2 3 4))
  (check-equal? (traversal-map vector-or-list-traversal #(1 2 3) add1) #(2 3 4))
  ; recursive use, no thunk needed
  (define rose-traversal-if (traversal-if list? (traversal-compose list-traversal rose-traversal-if) identity-traversal))
  (check-equal? (traversal-map rose-traversal-if '((1) ((2 3)) 4 5) add1)
                '((2) ((3 4)) 5 6))
  (check-equal? (traversal->list rose-traversal-if '((1) ((2 3)) 4 5)) '(1 2 3 4 5)))

(define (traversal-if/strict pred then-traversal else-traversal)
  (dependent-traversal (λ (target) (if (pred target) then-traversal else-traversal))))

(module+ test
  (let ([vector-or-list-traversal (traversal-if/strict list? list-traversal vector-traversal)])
    (check-equal? (traversal->list vector-or-list-traversal '(1 2 3)) '(1 2 3))
    (check-equal? (traversal->list vector-or-list-traversal #(1 2 3)) '(1 2 3))
    (check-equal? (traversal-map vector-or-list-traversal '(1 2 3) add1) '(2 3 4))
    (check-equal? (traversal-map vector-or-list-traversal #(1 2 3) add1) #(2 3 4))))

; like `cond` for `traversal-if`
; bodies are wrapped in lazy-traversal unless the only clause is an else clause.
(define-syntax traversal-cond
  (syntax-parser
    [(_ [(~literal else) body]) #'body]
    [(_ [pred body]) #'(traversal-if pred body (error 'traversal-cond "all traversal-cond predicates failed"))]
    [(_ [pred body] clause ...+) #'(traversal-if pred body (traversal-cond clause ...))]))

(module+ test
  (define rose-lv-traversal
    (traversal-cond
      [list? (traversal-compose list-traversal rose-lv-traversal)]
      [vector? (traversal-compose vector-traversal rose-lv-traversal)]
      [else identity-traversal]))
  (check-equal? (traversal-map rose-lv-traversal #((1) (#(2 3)) 4 5) add1)
                #((2) (#(3 4)) 5 6))
  (check-exn #rx"traversal-cond" (λ () (traversal-map (traversal-cond [(const #f) identity-traversal]) 1 add1))))

; A lazy traversal that isn't evaluated until it is used. It is evaluated at most once.
; Useful for creating recursive traversals.
(define-syntax-rule (lazy-traversal body ...) (promise-traversal/proc (lazy body ...)))

(define (promise-traversal/proc traversal-promise)
  (dependent-traversal (λ (target) (force traversal-promise))))

(module+ test
  (struct tree [value children] #:transparent)
  (define tree1 (tree 1 (list (tree 2 (list (tree 3 '())))
                              (tree 4 '()))))
  ; these aren't actually lenses, but that's just bc this module shouldn't depend on lens
  (define tree-value-lens (make-traversal (λ (proc tree) (proc (tree-value tree)))
                                          (λ (proc init tree) (proc (tree-value tree) init))))
  (define tree-children-lens (make-traversal (λ (proc tree) (proc (tree-children tree)))
                                             (λ (proc init tree) (proc (tree-children tree) init))))
  (define tree-values-traversal (lazy-traversal (traversal-append tree-value-lens (traversal-compose tree-children-lens list-traversal tree-values-traversal))))
  (check-equal? (traversal-foldl tree-values-traversal tree1 + 0) 10))

; A traversal that depends on its target. Useful for creating a conditional traversal. See `if-traversal`.
(define (dependent-traversal target->traversal)
  (make-traversal (λ (proc target) (traversal-map (target->traversal target) target proc))
                  (λ (proc init target) (traversal-foldl (target->traversal target) target proc init))))

(module+ test
  (let ([trv (dependent-traversal (λ (target) (if (vector? target) vector-traversal list-traversal)))])
    (check-equal? (traversal-map trv #(1 2 3) add1) #(2 3 4))
    (check-equal? (traversal-map trv '(1 2 3) add1) '(2 3 4))))

#;(-> traversal? target? (listof focus?))
; get a list of all foci
(define (traversal->list traversal target)
  (reverse (traversal-foldl traversal target cons '())))

(module+ test
  (test-equal? "traversal->list"
               (traversal->list (traversal-compose vector-traversal vector-traversal) #(#(1 2 3) #(4 5) #() #(6)))
               '(1 2 3 4 5 6)))



(module+ test
  (define lolol-traversal (traversal-compose list-traversal list-traversal list-traversal))
  (define lolol '(((1 2) (3)) ((4)) () (()())))
  (check-equal? (traversal-map lolol-traversal lolol add1) '(((2 3) (4)) ((5)) () (()()))))
