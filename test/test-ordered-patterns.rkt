#lang s-exp "hml-test-lang.rkt"

(define 2-ordered
  (ordered-sub-pat-matcher
   'a
   (ordered-sub-pat-matcher 'b 'm1)
   (ordered-sub-pat-matcher 'c 'm2)))


(test
 2-ordered
 '(a (b "one")
     (c "two"))
 ('m2 (a c) "two"
  'm1 (a b) "one"))

(test
 2-ordered
 ;; c & b in wrong order
 '(a (c "one")
     (b "two")))

(test
 2-ordered
 '(a (a (c "one")
        (b "two"))
     (b "1")
     (c "2")))

(test
 2-ordered
 '(a (a (b "one")
        (c "two"))
     (b "1")
     (c "2"))
 ('m1 (a a b) "one"
  'm2 (a a c) "two"))


;; TODO this problem shows the issue
;; we need to only self apply the very top level
;; possibly add a tl kw arg for self application
;; needs to be a member on the matcher-maker and not
;; at the decleration site

;; TODO this is actually incorrect
;; last two hashes should exist.
(test
 2-ordered
 '(a (a (b "one1")
        (c "two1"))
     (a (b "one2")
        (c "two2")))
 ('m1 (a a b) "one2"
  'm2 (a a c) "two2")
 ('m1 (a a b) "one1"
  'm2 (a a c) "two1"))

#;(test
 3-ordered
 '(a (a (b "one")
        (c "two"))
     (b "1")
     (c "2"))
 ('m1 (a a b) "one"
  'm2 (a a c) "two"))
