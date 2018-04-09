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
