#lang s-exp "hml-test-lang.rkt"

(define a-end-in-nest-b
  (make-pattern (a * (b 'one))))

;; (define last-in-a
;;   (make-pattern (a * 'last)))

;; (define thing-before-last-thing-c
;;   (make-pattern (a * 'thing (c *))))

;; (define thing-before-last-exact-c
;;   (make-pattern (a * 'thing (c 'left 'right))))

;; (define double-wildcard
;;   (make-pattern (a * * 'thing)))

;; (test
;;  a-end-in-nest-b
;;  '(a "a"))

;; (test
;;  a-end-in-nest-b
;;  '(a "b" "a"))

;; (test
;;  a-end-in-nest-b
;;  '(a (c "a")))

;; (test
;;  a-end-in-nest-b
;;  '(a (b "nope") "a"))

;; (test
;;  a-end-in-nest-b
;;  '(a (b "a"))
;;  ('a (a b) "a"))

;; (test
;;  a-end-in-nest-b
;;  '(a "still-matches" (b "a"))
;;  ('a (a b) "a"))

;; (test
;;  a-end-in-nest-b
;;  '(a "x" (b "a") "no-longer-matches"))

;; (test
;;  thing-before-last-thing-c
;;  '(a "a"))

;; (test
;;  thing-before-last-thing-c
;;  '(a "b" "a"))

;; (test
;;  thing-before-last-thing-c
;;  '(a (c "a")))

;; (test
;;  thing-before-last-thing-c
;;  '(a "matches!" (c "a"))
;;  ('thing (a) "matches!"))

;; (test
;;  thing-before-last-thing-c
;;  '(a "not-matches!" (c "a") "b"))

;; (test
;;  thing-before-last-thing-c
;;  '(a "matches!" (c "a" "b"))
;;  ('a (a) "matches!"))

;; (test
;;  thing-before-last-thing-c
;;  '(a "matches!" (c "a" (b "b")))
;;  ('a (a) "matches!"))

;; (test
;;  thing-before-last-thing-c
;;  '(a (b "nope") "a"))

;; (test
;;  thing-before-last-thing-c
;;  '(a (b "a")))

;; (test
;;  thing-before-last-thing-c
;;  '(a "no-matches" (b "a")))

;; (test
;;  thing-before-last-thing-c
;;  '(a "x" (b "a") "no-longer-matches"))

;; ; ---------------------------------------------------------------------------------------------------

;; (test
;;  thing-before-last-exact-c
;;  '(a "a"))

;; (test
;;  thing-before-last-exact-c
;;  '(a "b" "a"))

;; (test
;;  thing-before-last-exact-c
;;  '(a (c "a")))

;; (test
;;  thing-before-last-exact-c
;;  '(a (b "nope") "a"))

;; (test
;;  thing-before-last-exact-c
;;  '(a (b "a")))

;; (test
;;  thing-before-last-exact-c
;;  '(a "no-matches" (b "a")))

;; (test
;;  thing-before-last-exact-c
;;  '(a "x" (b "a") "no-longer-matches"))

;; (test
;;  thing-before-last-exact-c
;;  '(a "a"))

;; (test
;;  thing-before-last-exact-c
;;  '(a "b" "a"))

;; (test
;;  thing-before-last-exact-c
;;  '(a (c "a")))

;; (test
;;  thing-before-last-exact-c
;;  '(a "no matches!" (c "a")))

;; (test
;;  thing-before-last-exact-c
;;  '(a "not-matches!" (c "a") "b"))

;; (test
;;  thing-before-last-exact-c
;;  '(a "matches!" (c "a" "b"))
;;  ('thing (a)   "matches!"
;;   'left  (a c) "a"
;;   'right (a c) "b"))

;; (test
;;  thing-before-last-exact-c
;;  '(a "matches!" (c "a" (b "b")))
;;  ('thing (a)   "matches!"
;;   'left  (a c) "a"
;;   'right (a c) "<b>b</b>"))

;; (test
;;  thing-before-last-exact-c
;;  '(a "matches!" "wait-no" (c "a" (b "b"))))

;; (test
;;  thing-before-last-exact-c
;;  '(a (b "nope") "a"))

;; (test
;;  thing-before-last-exact-c
;;  '(a (b "a")))

;; (test
;;  thing-before-last-exact-c
;;  '(a "no-matches" (b "a")))

;; (test
;;  thing-before-last-exact-c
;;  '(a "x" (b "a") "no-longer-matches"))
