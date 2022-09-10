#lang info
(define collection "ocular-patdown")
(define deps '("base" "syntax-classes-lib" "https://github.com/michaelballantyne/bindingspec.git"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/main.scrbl" (multi-page) (experimental) "ocular-patdown")))
(define pkg-desc "A library for optics")
(define version "0.0")
(define pkg-authors '(mdelmonaco))
