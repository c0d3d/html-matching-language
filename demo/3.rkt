#lang racket

(require "../lib/hml-lang.rkt" xml)

(define a-doc
  (xexpr->xml
   '(a ((href "foo.com")) "bar")))

(match/html
 (a ((href foo.com)) 'content)
 a-doc
 content)
