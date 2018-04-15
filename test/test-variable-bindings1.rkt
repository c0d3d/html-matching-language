#lang s-exp "hml-test-lang.rkt"

(check-equal?
 (match/html
  (a (b 'm1)
     (c 'm2))
  (xexpr->xml
   '(a (b "one")
       (c "two")))
  (list (cons m1 m1.path) (cons m2 m2.path)))
 '((("one" . (a b)) ("two" . (a c)))))

;; c & b in wrong order
(check-equal?
 (match/html
  (a (b 'm1)
     (c 'm2))
  (xexpr->xml
   '(a (c "two")
       (b "one")))
  (list (cons m1 m1.path) (cons m2 m2.path)))
 '())

(check-equal?
 (match/html
  (a (b (c (d 'one 'two) 'three)
        'four)
     'five
     'six
     (e 'seven (f 'eight) 'nine))
  (xexpr->xml '(a))
  (list one one.path
        two two.path
        three three.path
        four four.path
        five five.path
        six six.path
        seven seven.path
        eight eight.path
        nine nine.path))
 '())
