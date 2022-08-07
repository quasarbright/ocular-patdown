#lang racket

(require (for-syntax syntax/parse syntax/parse/class/struct-id))
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
;
(struct lens (getter setter) #:constructor-name make-lens)
; (get target) retrieves the focus from the target
#;(-> any/c any/c)
; (-> set target new-focus) sets the focus in the target
#;(-> any/c any/c any/c)

(define (lens-get lens target) ((lens-getter lens) target))
(define (lens-set lens target focus) ((lens-setter lens) target focus))

#; (-> lens? any/c (-> any/c any/c) any/c)
; applies func to focus and updates target
(define (lens-modify lens target func) (lens-set lens target (func (lens-get lens target))))

#; lens?
; identity of lens composition. get returns the target, set replaces the target
(define identity-lens (make-lens identity (λ (target focus) focus)))

#; lens?
; lens targeting a cons and focusing on the car
(define car-lens (make-lens car (λ (pair a) (cons a (cdr pair)))))
#; lens?
; lens targeting a cons and focusing on the cdr
(define cdr-lens (make-lens cdr (λ (pair d) (cons (car pair) d))))

(module+ test
  (check-equal? (lens-get car-lens (cons 1 2)) 1)
  (check-equal? (lens-get cdr-lens (cons 1 2)) 2)
  (check-equal? (lens-set car-lens (cons 1 2) 3) (cons 3 2))
  (check-equal? (lens-set cdr-lens (cons 1 2) 3) (cons 1 3))
  (check-equal? (lens-modify car-lens (cons 1 2) -) (cons -1 2)))

#; (-> lens? lens? lens?)
; composes two lenses. outer-lens' focus should be inner-lens' target.
; for example, (lens-compose2 circle-center-lens posn-x-lens)
(define (lens-compose2 outer-lens inner-lens)
  (make-lens (λ (outer-target)
               (lens-get inner-lens (lens-get outer-lens outer-target)))
             (λ (outer-target inner-focus)
               (define outer-focus (lens-get outer-lens outer-target))
               (define inner-target outer-focus)
               (define new-inner-target (lens-set inner-lens inner-target inner-focus))
               (define new-outer-focus new-inner-target)
               (lens-set outer-lens outer-target new-outer-focus))))

(define caar-lens (lens-compose2 car-lens car-lens))
; it's flipped. function composition and lens composition are in opposite orders
(define cadr-lens (lens-compose2 cdr-lens car-lens))
(define cdar-lens (lens-compose2 car-lens cdr-lens))
(define cddr-lens (lens-compose2 cdr-lens cdr-lens))

(module+ test
  (define pair4 (cons (cons 1 2) (cons 3 4)))
  (check-equal? (lens-get caar-lens pair4) 1)
  (check-equal? (lens-set caar-lens pair4 'a) (cons (cons 'a 2) (cons 3 4)))
  (check-equal? (lens-modify caar-lens pair4 -) (cons (cons -1 2) (cons 3 4)))
  (check-equal? (lens-get cadr-lens pair4) (cadr pair4))
  (check-equal? (lens-set cadr-lens pair4 'a) (cons (cons 1 2) (cons 'a 4)))
  (check-equal? (lens-modify cadr-lens pair4 -) (cons (cons 1 2) (cons -3 4))))

#; (-> lens? ... lens?)
; composes lenses, "outer" lenses first
(define (lens-compose . lenses)
  (foldr lens-compose2 identity-lens lenses))

(module+ test
  (check-equal? (lens-get (lens-compose cdr-lens car-lens) pair4) (cadr pair4)))

; creates a lens that focuses on a field of a struct.
; for fields that are part of a super type, supply #:parent.
; NOTE: if you use a super-type's lens to update a subtype, you will get an instance of the super-type, not the sub-type.
(define-syntax struct-lens
  (syntax-parser
    [(_ struct-name:struct-id field-name:id (~optional (~seq #:parent parent-struct-name:struct-id)))
     #'(make-lens
        (λ (target) (match target [(struct* (~? parent-struct-name struct-name) ([field-name focus])) focus]))
        (λ (target new-focus) (struct-copy struct-name target [field-name (~? (~@ #:parent parent-struct-name) (~@)) new-focus])))]))

(module+ test
  (struct posn [x y] #:transparent)
  (define posn-x-lens (struct-lens posn x))
  (define posn-y-lens (struct-lens posn y))
  (define posn34 (posn 3 4))
  (check-equal? (lens-get posn-x-lens posn34) 3)
  (check-equal? (lens-set posn-x-lens posn34 5) (posn 5 4))
  (check-equal? (lens-modify posn-x-lens posn34 -) (posn -3 4))
  (struct posn3 posn [z] #:transparent)
  (define posn3-z-lens (struct-lens posn3 z))
  (define posn3-x-lens (struct-lens posn3 x #:parent posn))
  (define posn345 (posn3 3 4 5))
  (check-equal? (lens-get posn3-z-lens posn345) 5)
  (check-equal? (lens-set posn3-z-lens posn345 6) (posn3 3 4 6))
  (check-equal? (lens-modify posn3-z-lens posn345 -) (posn3 3 4 -5))
  (check-equal? (lens-get posn-x-lens posn345) 3)
  ; using a super-type's lens to update an instance of subtype results in an instance of the supertype
  (check-equal? (lens-set posn-x-lens posn345 1) (posn 1 4))
  (check-equal? (lens-modify posn-x-lens posn345 -) (posn -3 4))
  (check-equal? (lens-get posn3-x-lens posn345) 3)
  (check-equal? (lens-set posn3-x-lens posn345 1) (posn3 1 4 5))
  (check-equal? (lens-modify posn3-x-lens posn345 -) (posn3 -3 4 5)))

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
  (check-equal? (update (posn 1 2) [(struct-field posn x a) (set a 3)]) (posn 3 2))
  (check-equal? (update (posn 1 2) [(struct-field posn x) (set x 3)]) (posn 3 2))
  (check-equal? (update (posn (cons 1 2) 3) [(struct-field posn x (cons a b)) (modify a -) (modify b sqr)]) (posn (cons -1 4) 3)))
