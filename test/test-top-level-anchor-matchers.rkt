#lang s-exp "hml-test-lang.rkt"

;; (define a-b-c-anchor
;;   (top-level-anchored-matcher
;;    'a
;;    (top-level-anchored-matcher
;;     'b
;;     (top-level-anchored-matcher
;;      'c
;;       'inside))))

;; (test
;;  a-b-c-anchor
;;  '(a (b (c (d "String"))))
;;  ('inside (a b c) "<d>String</d>"))

;; (define a-anchor
;;   (top-level-anchored-matcher
;;    'a
;;    'inside))

;; (test
;;  a-anchor
;;  '(a (b "stuff"))
;;  ('inside (a) "<b>stuff</b>"))

;; ; Should not match, a not top level
;; (test
;;  a-anchor
;;  '(b (a "stuff")))

;; (test
;;  a-anchor
;;  '(a (a "stuff"))
;;  ('inside (a) "<a>stuff</a>"))

;; (define a-a-anchor
;;   (top-level-anchored-matcher
;;    'a
;;    (top-level-anchored-matcher
;;     'a
;;     'inside)))

;; (test
;;  a-a-anchor
;;  '(a (a "stuff"))
;;  ('inside (a a) "stuff"))

;; (test
;;  a-a-anchor
;;  '(a (a (a "stuff")))
;;  ('inside (a a) "<a>stuff</a>"))

;; ; Should not match, only one top level a
;; (test
;;  a-a-anchor
;;  '(a "stuff"))

;; ;; Testing top-level and sub matchers together

;; (define a-anchor-a-sub
;;   (top-level-anchored-matcher
;;    'a
;;    (sub-pat-matcher
;;     'a
;;     'inside)))

;; (test
;;  a-anchor-a-sub
;;  '(a "stuff"))

;; (test
;;  a-anchor-a-sub
;;  '(a (a "stuff"))
;;  ('inside (a a) "stuff"))

;; (test
;;  a-anchor-a-sub
;;  '(a (a (a "stuff")))
;;  ('inside (a a a) "stuff")
;;  ('inside (a a) "<a>stuff</a>"))

;; (test
;;  a-anchor-a-sub
;;  '(a (b (a "stuff")))
;;  ('inside (a b a) "stuff"))

;; (define a-anchor-a-sub-a-anchor
;;   (top-level-anchored-matcher
;;    'a
;;    (sub-pat-matcher
;;     'a
;;     (top-level-anchored-matcher
;;      'a
;;      'inside))))

;; (test
;;  a-anchor-a-sub-a-anchor
;;  '(a (b (a (a "stuff"))))
;;  ('inside (a b a a) "stuff"))

;; (test
;;  a-anchor-a-sub-a-anchor
;;  '(a (b (a (b (a "stuff"))))))

;; (test
;;  a-anchor-a-sub-a-anchor
;;  '(a (b (a (a (b (a (a "stuff")))))))
;;  ('inside (a b a a b a a) "stuff")
;;  ('inside (a b a a) "<b><a><a>stuff</a></a></b>"))

;; (test
;;  a-anchor-a-sub-a-anchor
;;  '(a (a (a (a (a "stuff")))))
;;  ('inside (a a a a a) "stuff")
;;  ('inside (a a a a) "<a>stuff</a>")
;;  ('inside (a a a) "<a><a>stuff</a></a>"))

;; ; No top level a after sub a
;; (test
;;  a-anchor-a-sub-a-anchor
;;  '(a (b (a (b (a "stuff"))))))

;; (test
;;  a-anchor-a-sub-a-anchor
;;  '(a (a (b (a "stuff")))))

;; (test
;;  a-anchor-a-sub-a-anchor
;;  '(a (a "stuff")))
