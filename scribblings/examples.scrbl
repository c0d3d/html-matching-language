#lang scribble/manual

@(require
  (for-label "../lib/hml-lang.rkt" racket)
  scribble/eval)

@title{HML Examples}

@(define do-eval (make-base-eval))
@interaction-eval[#:eval do-eval
                  (require "../lib/hml-lang.rkt")
                  (define doc1
                    (xexpr->xml
                     '(html
                       (head
                        (title "Example Page"))
                       (body
                        (div
                         (h1 "Section 1")
                         (p "Text 1"))
                        (div
                         (h1 "Section 2")
                         (p "Text 2"))
                        (p
                         (h1 "Unmatched")
                         (p "More text"))))))]

@examples[
          #:eval do-eval

          (match/html
           (div (h1 'title)
                (p  'content))
           doc1
           (cons (hash-ref mm 'title) (hash-ref mm 'content)))
          ]
