#lang scribble/manual

@;{------------------------------------------------------------------------------------------------}
@;{Requirements} 

@(require (for-label "../lib/hml-lang.rkt" racket xml)
          scribble/eval)
@(declare-exporting "../lib/hml-lang.rkt")

@;{------------------------------------------------------------------------------------------------}
@;{Examples Setup} 

@(define do-eval (make-base-eval))
@interaction-eval[#:eval do-eval
                  (require "../lib/hml-lang.rkt" xml)
                  (define doc1
                    (xexpr->xml
                     '(body
                        (div
                         (h1 "Section 1")
                         (p "Text 1"))
                        (div
                         (h1 "Section 2")
                         (p "Text 2")))))]

@title{HML Specification}

@section[#:tag "pat-stx"]{Patterns}

HML allows programmers to embed pattern literals into their program using @racket[make-pattern].

The grammar for patterns is as follows:

@defform/subs[(make-pattern pattern)
              [(pattern sub-pattern special pat-var)
               (sub-pattern (id pattern ...))
               (special *)
               (pat-var symbol)]]{
 This form constructs a pattern value matching the pattern literal provided in @racket[pattern].
}

Example:
@codeblock{
 (make-pattern
 (div (h1 'title)
 (p  'contents)))
}

@section[#:tag "pat-semantics"]{Pattern Semantics}

@section[#:tag "match-stx"]{Match Syntax}

HML provides the @racket[match/html] form to use patterns. The form takes in a pattern, a document,
and a body.

The grammar for pattern match expressions is as follows:

@defform/subs[(match/html pattern doc body)
              [(pattern pattern)
               (doc any-racket-expression)
               (body any-racket-expression)]]{
 This form constructs a match value in the same structure as @racket[body] and with the match content
 matched in @racket[pattern].
}

There are several ways to retrieve match content while inside the body which we will go over in
@secref["match-content"].

@section[#:tag "match-semantics"]{Match Semantics}

@section[#:tag "match-content"]{Match Content}

There are two ways of retrieving matched content when in the body of a match expression. The first way
is by using the @racket[match-map]. When the body of a match expression is run, a map object named mm
is added to the scope. This match map contains references to all matched content in the form of
match-data structs.

Given the following document bound to @code{doc1}:
@interaction[
	#:eval do-eval
	(display-xml/content doc1)
]

@examples[
          #:eval do-eval

          (match/html
           (div (h1 'title)
                (p  'content))
           doc1
           (cons (hash-ref mm 'title) (hash-ref mm 'content)))
          ]

An easier way to extract match content in the body of a match expression is through variable name
bindings for pattern literals. When a match expression is run, variable names cooresponding to the
matched pattern literals are added to the body expression's scope. This allows the user to refer to
match content directly in the body. In addition, the path to a matched literal can be accessed by
adding `.path` to a matched literal variable.

@examples[
          #:eval do-eval

          (match/html
           (div (h1 'title)
                (p 'content))
           doc1
           (cons title.path content))
          ]