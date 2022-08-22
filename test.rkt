#lang racket

(require "./main.rkt")

(update (list 1 2) [(cons a _) (set a #t)])
