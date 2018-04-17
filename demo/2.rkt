#lang racket

(require "../lib/hml-lang.rkt" xml)

(define a-doc
  (xexpr->xml
   '(b (a "1") (a "2" (a "3")) (a "4"))))

(match/html
 (a 'content)
 a-doc
 (list content content.path))
 