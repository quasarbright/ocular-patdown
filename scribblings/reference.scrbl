#lang scribble/manual

@require[@for-label[ocular-patdown/update]]

@title[#:style 'toc #:tag "reference"]{Optics Reference}

@defmodule[ocular-patdown]

The @racketmodname[ocular-patdown] module provides all bindings of @racketmodname[ocular-patdown/optics] and @racketmodname[ocular-patdown/update].

@local-table-of-contents[]

@include-section[(lib "ocular-patdown/scribblings/optics.scrbl")]
@include-section[(lib "ocular-patdown/scribblings/lens.scrbl")]
@include-section[(lib "ocular-patdown/scribblings/traversal.scrbl")]
@include-section[(lib "ocular-patdown/scribblings/isomorphism.scrbl")]
@include-section[(lib "ocular-patdown/scribblings/prism.scrbl")]
@include-section[(lib "ocular-patdown/scribblings/update.scrbl")]
