#lang s-exp "hml-test-lang.rkt"

(define a-pat-matcher (sub-pat-matcher 'a 'content))

(test
 a-pat-matcher
 '(a "Stuff")
 ('content (a) "Stuff"))

(test
 a-pat-matcher
 '(a (a "Stuff"))
 ('content (a a) "Stuff")
 ('content (a) "<a>Stuff</a>"))


(test
 a-pat-matcher
 '(a (a "Stuff" (b (a "Other"))))
 ('content (a a b a) "Other")
 ('content (a a) "Stuff<b><a>Other</a></b>")
 ('content (a) "<a>Stuff<b><a>Other</a></b></a>"))

(test
 (b 'content)
 '(a (a "Stuff" (b (a "Other"))))
 ('content (a a b) "<a>Other</a>"))

(test
 (a (b 'content))
 '(a (b (a (b "Hello" "World!"))))
 ('content (a b a b) "HelloWorld!")
 ('content (a b) "<a><b>HelloWorld!</b></a>"))

;; Combined stuff is not implemented yet.
#;(test
 (a 'one (b 'content))
 '(a (b "Hello"))
 ())
#;(test
 (a 'one (b 'content))
 '(a (h1 "Some title")
     (b "Hello"))
 ('one (a) "<h1>Some title</h1>"
  'content (a b) "Hello"))

(test
 a-pat-matcher
 '(a (h1 "Title!")
     (p "test"))
 ('content (a) "<h1>Title!</h1><p>test</p>"))


(test
 a-pat-matcher
 '(a (h1 "Title!")
     (p "test"))
 ('content (a) "<h1>Title!</h1><p>test</p>"))