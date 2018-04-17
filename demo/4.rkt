#lang racket

(require "../lib/hml-lang.rkt" xml)

(define a-doc
  (xexpr->xml
   '(h1 (div (p
              ((key "value") (extra-key "extra-value"))
              (b (s (h1 (div (p ((key "value"))
                                (c "Hello")
                                (d "Goodbye"))))
                    (h1 (div (p ((key "wrong-value"))
                                (c "Word")
                                (d "Another"))))))
              (h1 (div (p "Wow"))))))))

(match/html
 (h1 (div (p ((key value))
             'contents1
             'contents2)))
 a-doc
 (list contents1 contents2))