#lang scribble/manual

@require[@for-label[ocular-patdown/update]]

@title{Ocular Patdown}
@author[@author+email["Mike Delmonaco" "mdelmonacochs@gmail.com"]]

@defmodule[ocular-patdown]

This is a (currently experimental) library for @tech{optic}s. Optics are useful for performing deep immutable updates and accesses within structures.
This library also provides the @racket[update] form, which is like match, but can be used for immutable updates as well.
The @racket[update] form does not require knowledge of optics to use.

@local-table-of-contents[]

@include-section[(lib "ocular-patdown/scribblings/optics.scrbl")]
@include-section[(lib "ocular-patdown/scribblings/lens.scrbl")]
@include-section[(lib "ocular-patdown/scribblings/update.scrbl")]
