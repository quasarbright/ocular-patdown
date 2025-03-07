#lang info
(define collection "ocular-patdown")
(define license 'MIT)
(define deps '("base" "syntax-classes-lib" "syntax-spec-v3"))
(define build-deps '("scribble-lib" "racket-doc" "rackunit-lib"))
(define scribblings '(("scribblings/main.scrbl" (multi-page) (experimental) "ocular-patdown")))
(define pkg-desc "A library for optics")
(define version "0.0")
(define pkg-authors '(mdelmonaco))
