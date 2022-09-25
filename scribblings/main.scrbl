#lang scribble/manual

@title{Ocular Patdown}
@author[@author+email["Mike Delmonaco" "mdelmonacochs@gmail.com"]]

This is a (currently experimental) library for @tech{optic}s. Optics are useful for performing deep immutable updates and accesses within structures.
This library also provides the @racket[update] form, which is like match, but can be used for immutable updates as well.
The @racket[update] form does not require knowledge of optics to use.

For those unfamiliar with lenses, read the @seclink["optics-guide"]{Optics Guide}.

@local-table-of-contents[]

@include-section[(lib "ocular-patdown/scribblings/guide.scrbl")]
@include-section[(lib "ocular-patdown/scribblings/reference.scrbl")]
