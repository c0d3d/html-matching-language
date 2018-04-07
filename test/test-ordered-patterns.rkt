#lang s-exp "hml-test-lang.rkt"

(define 2-ordered
  (make-pattern (a (b 'm1)
                   (c 'm2))))

;; (test
;;  2-ordered
;;  '(a (b "one")
;;      (c "two"))
;;  ('m2 (a c) "two"
;;   'm1 (a b) "one"))

;; (test
;;  2-ordered
;;  ;; c & b in wrong order
;;  '(a (c "one")
;;      (b "two")))

;; (test
;;  2-ordered
;;  '(a (a (c "one")
;;         (b "two"))
;;      (b "1")
;;      (c "2")))

;; (test
;;  2-ordered
;;  '(a (a (b "one")
;;         (c "two"))
;;      (b "1")
;;      (c "2"))
;;  ('m1 (a a b) "one"
;;   'm2 (a a c) "two"))

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

;; (test
;;  (a (b 'content1 'content2))
;;  '(a (b (a (b "Hello" "World!"))))
;;  ('content1 (a b a b) "Hello"
;;   'content2 (a b a b) "World!"))

#;(test
 3-ordered
 '(a (a (b "one")
        (c "two"))
     (b "1")
     (c "2"))
 ('m1 (a a b) "one"
  'm2 (a a c) "two"))
