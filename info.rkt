#lang info
(define collection "ocular-patdown")
(define deps '("base" "syntax-classes-lib" "https://github.com/michaelballantyne/bindingspec.git#b1e6f3c42556e898e983f1b39595c02357a0406c"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/pattern-updating.scrbl" ())))
(define pkg-desc "match-like syntax for deep immutable updates")
(define version "0.0")
(define pkg-authors '(mdelmonaco))
