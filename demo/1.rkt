#lang racket

(require "../lib/hml-lang.rkt" xml)

(define a-doc
  (xexpr->xml
   '(a "foo")))

(match/html
 (a 'content)
 a-doc
 (list content content.path))