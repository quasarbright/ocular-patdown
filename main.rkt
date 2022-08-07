#lang racket

(require (for-syntax syntax/parse syntax/parse/class/struct-id))
(require "./private/lens.rkt")
(module+ test (require rackunit))

#|
; happy birthday dad!
(update parents
  [(list (person dad-name dad-age) (person mom-name mom-age))
   (set dad-age (add1 (get dad-age))])
; returns a copy of the same list, but dad's age is incremented

going through the pattern binds optics to variable names
executing the body should run the specified updates and return the updated target (no mutation)

this could be achieved by making get, set, and modify curried functions that are closed over the target:
(define (set lens value) (lens-set lens value target))

in order to allow definitions in the body of the clause, (get ...) to return results that reflect recent modifications, and sets and modifications to work, you can make target a variable and set! it

(update (list 7 8 9)
  [(list a b c)
   (set a 1)
   (set b (get a))
   (define x (get c))
   (set c (add1 x))])
> (list 1 1 10)

in general, for single-clause:

(define target ...the target)
(define (set lens value) (set! target (lens-set lens value target)))
(define (modify lens func) (set! target (lens-modify lens func target)))
(define (get lens) (lens-get lens target))
... define lenses for a,b,c
... body
target

for multi-clause, you'll want to make sure the mutation is isolated to each clause in case mid-way failures are possible
|#

; --- lenses ---

; --- core compiler ---

(define-syntax update
  (syntax-parser
    [(_ target-expr [pat body ...])
     ;; (define/syntax-parse get (syntax-local-introduce #'get))
     ;; (define/syntax-parse set (syntax-local-introduce #'set))
     ;; (define/syntax-parse modify (syntax-local-introduce #'modify))
     (with-syntax
       ([get (syntax-local-introduce #'get)]
        [set (syntax-local-introduce #'set)]
        [modify (syntax-local-introduce #'modify)])
       #'(let () ; this is used to create a definition context
           ; for multi-clause, make sure you isolate the mutations within each clause
           (define target-var target-expr)
           (define (get lens) (lens-get lens target-var))
           (define (set lens focus) (set! target-var (lens-set lens target-var focus)))
           (define (modify lens func) (set! target-var (lens-modify lens target-var func)))
           (update* target-var pat identity-lens (begin body ... target-var))))]))

; compile the patterns to nested bindings of variables to lenses
(define-syntax update*
  (syntax-parser
    [(_ target-var:id pat lens-so-far-var:id body)
     (syntax-parse #'pat
       #:datum-literals (cons list struct-field _)
       [(cons car-pat cdr-pat)
        #'(if (cons? target-var)
              (let ([car-val (car target-var)]
                    [cdr-val (cdr target-var)]
                    [lens-with-car (lens-compose lens-so-far-var car-lens)]
                    [lens-with-cdr (lens-compose lens-so-far-var cdr-lens)])
                (update* car-val car-pat lens-with-car
                         (update* cdr-val cdr-pat lens-with-cdr
                                  body)))
              (error 'update "expected a cons, got ~a" target-var))]
       [(list) #'(update* target-var _ lens-so-far body)]
       [(list pat0 pat ...)
        #'(update* target-var (cons pat0 (list pat ...)) lens-so-far-var body)]
       [(struct-field struct-name:struct-id field-name:id (~optional field-pat #:defaults ([field-pat #'field-name])))
        (define/syntax-parse struct-name? (get-struct-pred-id #'struct-name))
        #'(if (struct-name? target-var)
              (let* ([field-lens (struct-lens struct-name field-name)]
                     [field-value (lens-get field-lens target-var)]
                     [new-lens (lens-compose lens-so-far-var field-lens)])
                (update* field-value field-pat new-lens body))
              (error 'update "expected a ~a, got ~a" 'struct-name? target-var))]
       [_ #'body]
       [var:id #'(let ([var lens-so-far-var]) body)])]))

(define-for-syntax (get-struct-pred-id struct-id-stx)
  (syntax-parse struct-id-stx
    [s:struct-id
     (attribute s.predicate-id)]))

(module+ test
  (check-equal? (update (list 1 2) [(cons a (cons b _)) (set a #t) (set b #f)])
                (list #t #f))
  (check-equal? (update (list 1 2) [(list a b) (set a #t) (set b #f)])
                (list #t #f))
  (check-equal? (update (list 7 8 9)
                        [(list a b c)
                         (set a 1)
                         (set b (get a))
                         (define x (get c))
                         (set c (add1 x))])
                (list 1 1 10))
  (struct posn [x y] #:transparent)
  (check-equal? (update (posn 1 2) [(struct-field posn x a) (set a 3)]) (posn 3 2))
  (check-equal? (update (posn 1 2) [(struct-field posn x) (set x 3)]) (posn 3 2))
  (check-equal? (update (posn (cons 1 2) 3) [(struct-field posn x (cons a b)) (modify a -) (modify b sqr)]) (posn (cons -1 4) 3)))
