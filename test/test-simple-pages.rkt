#lang s-exp "hml-test-lang.rkt"

(define a-pat-matcher (make-pattern (a 'content)))

(test
 a-pat-matcher
 '(a "Stuff")
 ('content (a) "Stuff"))

(test
 a-pat-matcher
 '(a (a "Stuff"))
 ('content (a) "<a>Stuff</a>")
 ('content (a a) "Stuff"))


(test
 a-pat-matcher
 '(a (a "Stuff" (b (a "Other"))))
 ('content (a) "<a>Stuff<b><a>Other</a></b></a>")
 ('content (a a b a) "Other"))


(test
 (b 'content)
 '(a (a "Stuff" (b (a "Other"))))
 ('content (a a b) "<a>Other</a>"))

(test
 a-pat-matcher
 '(a (h1 "Title!")
     (p "test")))

;; Check for matching direct data (none should pass)

(test
 a-pat-matcher
 '<!--a-->) ; Comment

(test
 a-pat-matcher
 "Hello") ; String

(test
 a-pat-matcher
 '(b "a")) ; Wrong tag

(define pat1 (make-pattern (h1 (div (p 'contents)))))
(test
 pat1
 '(p (a "string")
     (b (h1 (div (p "Some junk")))))
 ('contents (p b h1 div p) "Some junk"))

(test
 (h1 (div (p 'contents1)))
 '(h1 (div (p
            (b (s (h1 (div (p (c "Hello")
                              (d "Goodbye"))))
                  (h 2)))
            (h1 (div (p "Wow"))))))
 ('contents1 (h1 div p h1 div p) "Wow"))

(test
 (h1 (div (p 'contents1 'contents2)))
 '(h1 (div (p
            (b (s (h1 (div (p (c "Hello")
                              (d "Goodbye"))))
                  (h 2)))
            (h1 (div (p "Wow"))))))
  ('contents1 (h1 div p) (string-append
                          "<b><s><h1><div><p><c>Hello</c>"
                          "<d>Goodbye</d></p></div></h1>"
                          "<h>&#2;</h></s></b>")
   'contents2 (h1 div p) "<h1><div><p>Wow</p></div></h1>")
  ('contents1 (h1 div p b s h1 div p) "<c>Hello</c>"
   'contents2 (h1 div p b s h1 div p) "<d>Goodbye</d>"))


(test
 'hello
 '(a (b (c "one")
        (d "two"))
    "three")
 ('hello () "<a><b><c>one</c><d>two</d></b>three</a>")
 ('hello (a) "three")
 ('hello (a) "<b><c>one</c><d>two</d></b>")
 ('hello (a b) "<d>two</d>")
 ('hello (a b) "<c>one</c>")
 ('hello (a b c) "one")
 ('hello (a b d) "two"))


(test
 (a (b (c (d (e (f (g 'one))))))
    (h (i (j (k (l (m 'two))))))
    (b (c (d (e (f (g 'three)))))))
 '(a (b (c (d (e (f (g "one"))))))
     (h (i (j (k (l (m "two"))))))
     (b (c (d (e (f (g "three")))))))
 ('one   (a b c d e f g) "one"
  'two   (a h i j k l m) "two"
  'three (a b c d e f g) "three"))

(test
 (a (b (c (d (e (f (g 'one))))))
    (h (i (j (k (l (m 'two))))))
    (b (c (d (e (f (g 'three)))))))
 '(a (b (c (d (e (f (g "one"))))))
     (h (i (j (k (l (m "two" "oops"))))))
     (b (c (d (e (f (g "three"))))))))

(test
 (a (a (a 'nested)))
 '(c (a (a (d (a (b (a (a (a "three")
                          (a (a (a "Nested"))))
                       (a (a "one" "two")
                          (a "three'"
                             (a (a (a (a (a (a "Nested²")))))))
                          (a (a "one" "two")
                             (a "three''")
                             (a (a (a "Nested 2"))))))))))))

 ('nested (c a a d a b a a a a a) "Nested")
 ('nested (c a a d a b a a a a a a) "<a><a><a>Nested²</a></a></a>")
 ('nested (c a a d a b a a a a a a a) "<a><a>Nested²</a></a>")
 ('nested (c a a d a b a a a a a a a a) "<a>Nested²</a>")
 ('nested (c a a d a b a a a a a a a a a) "Nested²")
 ('nested (c a a d a b a a a a a a) "Nested 2"))

(test
 (a (b 'one (c 'two))
    'three)
 '(a (b "1" (c "2"))
    "3")
 ('one (a b) "1"
  'two (a b c) "2"
  'three (a) "3"))


(test
 (a (b 'one (c 'two))
    'three)
 '(a (b "1" (c "2" "oops"))
    "3"))

(test
 (a (b 'one (c 'two))
    'three)
 '(a (b "1" "2" (c "2"))
    "3"))


(test
 (a (b 'one (c 'two))
    'three)
 '(a (d "1" (c "2" "oops"))
    "3"))
