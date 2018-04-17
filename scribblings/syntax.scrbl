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

This is the grammar for pattern literals in HML.

@(racketgrammar* #:literals (*)
                 [pattern sub-pattern special pat-var]
                 [sub-pattern (id [maybe-attrs] pattern ...)]
                 [maybe-attrs ((id id) ...)]
                 [special *]
                 [pat-var symbol])

See @secref["pat-semantics"] for information on semantics of embedded patterns.

@section[#:tag "forms"]{Forms}

HML provides two additional syntactic forms used for constructing patterns, and matching them against
documents.

@defform[(make-pattern pattern)]{
 The @racket[make-pattern] form constructs a pattern value from an embedded pattern literal.
 @racket[pattern] should conform to the grammar specified in @secref["pat-stx"].

 Example:
 @codeblock{
  (make-pattern (div (h1 'title)
                     (p  'contents)))
 }
}


@defform[(match/html pattern-or-id document body ...+)]{
 This form performs a matching acion (as described in @secref["pat-semantics"]) against the
 given @racket[pattern-or-id]. If @racket[pattern-or-id] is a plain identifier @racket[match/html] will
 attempt to use it as one (note that this means that if it is unbound, the program will fail to
 compile). Otherwise @racket[pattern-or-id] is taken to be an embedded pattern literal whose grammar
 needs to conform to the normal pattern grammar defined in @secref["pat-stx"]. The result of an
 @racket[match/html] expression is a list with the result of evaluating @racket[body ...] for each
 match that was discovered in the document.

 If you embed a pattern literal inside @racket[match/html], it will automatically bind the value of
 each match to an identifier with the same name inside the body. Additionally, it will bind the path
 to reach the matched value inside the @racket[{name}.path] identifier. For example, if you had a
 pattern variable named @racket[p], inside the body, there would be a identifier named @racket[p.path]
 which will be bound to the path that the variable was reached at for each match.

 See more information about value extraction in @secref["match-content"].

 Example:
 @codeblock{
  (match/html
   (div (h1 'title)
        (p  'contents))
   element
   (cons contents.path (cons title title.path)))
 }
}

@section[#:tag "pat-semantics"]{Pattern Semantics}

Patterns are composed of either pattern variables (which are represented by quoted identifiers), or
sub-patterns. Sub-patterns are used to match the nesting structure of xml, and contain a tag name as
the first element. Sub-patterns can optionally contain a set of attributes, which are matched against
the xml attributes of an element. Sub-patterns only match xml that has the same tag name, contains
attributes with the same values (note @italic{contains}, an element with extra attributes can still
match), has the same number of sub-elements as the xml, and each sub-element matches the sub-elements
of the xml. Pattern variables match against everything, and will capture a
stringified version of the xml that is matched in its relative position. Their match is placed into
the match map with a key that is there name. 

A matching action is an attempt to match the pattern against a given document, and capture each
individual match for use later.

Top level patterns are applied recursively at each level in the xml, and on each element.
          

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