#lang scribble/manual

@title{The HTML Matching Language (HML)}

The HTML Matching Language (HML) is an eDSL in Racket allowing users to write HTML matchers.
Matchers allow for robust and easy destructuring of HTML documents.

The key strengths of these patterns are due to there loose coupling to the underlying document that
is being matched. Patterns are made out of compisition of smaller patterns, and the library will
do some "spelunking" through the document in order to identify matching sub documents.

@include-section["examples.scrbl"]
