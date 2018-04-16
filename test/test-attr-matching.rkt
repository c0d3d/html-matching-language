#lang s-exp "hml-test-lang.rkt"

(define a-pat-matcher (make-pattern (a 'content)))

(test
 a-pat-matcher
 '(a ((a "stuff")) "Stuff")
 ('content (a) "Stuff"))

(test
 (a ((a stuff)) 'content)
 '(a "Stuff"))

(test
 (a ((a stuff)) 'content)
 '(a ((a "stuff")) "Stuff")
 ('content (a) "Stuff"))


(test
 (a ((key value)) 'content)
 '(a ((key "value")) (a "Stuff"))
 ('content (a) "<a>Stuff</a>"))

(test
 (a ((key value)) 'content)
 '(a (a ((key "value")) "Stuff"))
 ('content (a a) "Stuff"))

(test
 (a ((key value)) 'content)
 '(a (a "Stuff" (b ((key "oops")) (a "Other")))))

(test
 (a ((key value)) 'content)
 '(a ((key "oops")) (a "Stuff" (b ((key "oops")) (a ((key "value")) "Other"))))
 ('content (a a b a) "Other"))

(test
 (a ((key value)) 'content)
 '(a ((key "oops")) (a "Stuff" (b ((key "oops")) (a "Other")))))

(test
 (a ((key value)) 'content)
 '(a (a "Stuff" (b ((key "oops")) (a ((key "value")) "Other"))))
 ('content (a a b a) "Other"))

(test
 (a ((key value)) 'content)
 '(a (a "Stuff" (b ((key "oops")) (a ((other "a") (key "value")) "Other"))))
 ('content (a a b a) "Other"))

(test
 (a ((key value)) 'content)
 '(a ((other "a") (key "value")) (a "Stuff" (b ((key "oops")) (a "Other"))))
 ('content (a) "<a>Stuff<b key=\"oops\"><a>Other</a></b></a>"))

(test
 (a ((key value)) 'content)
 '(a ((other "a") (key "value")) (a "Stuff" (b ((key "oops")) (a ((a "b")) "Other"))))
 ('content (a) "<a>Stuff<b key=\"oops\"><a a=\"b\">Other</a></b></a>"))
