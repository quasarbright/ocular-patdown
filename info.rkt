#lang info
(define collection "ocular-patdown")
(define deps '("base" "syntax-classes-lib" "https://github.com/michaelballantyne/bindingspec.git"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/ocular-patdown.scrbl" ())))
(define pkg-desc "match-like syntax for deep immutable updates")
(define version "0.0")
(define pkg-authors '(mdelmonaco))
