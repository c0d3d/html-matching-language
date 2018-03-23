#lang s-exp "hml-test-lang.rkt"

(define a-b-c-⚓
  (top-level-anchored-matcher
   'a
   (top-level-anchored-matcher
    'b
    (top-level-anchored-matcher
     'c
     (top-level-anchored-matcher
      #f
      'inside)))))


#;(test
 a-b-c-⚓
 '(a (b (c (d "String"))))
 ('inside (a b c) "<d>String</d>"))
