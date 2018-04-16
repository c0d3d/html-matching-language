#lang scribble/manual

@(require (for-label "../lib/hml-lang.rkt"))
@(declare-exporting "../lib/hml-lang.rkt")
@title{HML Specification}

@section[#:tag "pat-stx"]{Pattern Syntax}

HML allows programmers to embed pattern literals into their program using the @racket[make-pattern], and @racket[match/html].

The grammar for patterns is as follows:

@defform/subs[(make-pattern pattern)
              [(pattern sub-pattern special pat-var)
               (sub-pattern (id pattern ...))
               (special *)
               (pat-var symbol)]]{
  This form constructs a pattern value matching the pattern literal provided in @racket[pattern].
  See @secref["pat-semantics"] for details on the semantics of the produced pattern value.
}

@section[#:tag "pat-semantics"]{Pattern Sematics}

