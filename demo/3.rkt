#lang racket

(require "../lib/hml-lang.rkt" xml)

(define a-doc
  (xexpr->xml
   '(div (a ((href "foo.com")) "yes")
         (a ((href "bar.com")) "no"))))

(match/html
 (a ((href foo.com)) 'content)
 a-doc
 content)
