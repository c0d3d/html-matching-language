#lang racket

(require
 rackunit
 racket/match
 "mdm-test-lib.rkt"
 "../lib/multi-diff-map.rkt"
 "../lib/html-matcher.rkt"
 xml
 racket/hash)

(define (remove-loc x)
  (cond
    [(location? x) #f]
    [(document? x)
     (document (remove-loc x) (remove-loc x) (remove-loc x))]
    [(element? x)
     (element
      #f #f (element-name x)
      (map remove-loc (element-attributes x))
      (map remove-loc (element-content x)))]
    [(attribute? x) (attribute #f #f (attribute-name x) (attribute-value x))]
    [(entity? x) (entity #f #f (entity-text x))]
    [(pcdata? x) (pcdata #f #f (pcdata-string x))]
    [(cdata? x) (cdata #f #f (cdata-string x))]
      [else x]))

;; has-tag?
(check-false (has-tag? (pcdata #f #f "a") 'a))
(check-false (has-tag? (cdata #f #f "B") 'B))
(check-false (has-tag? (attribute #f #f 'a "b") 'a))
(check-false (has-tag? (element #f #f 'p '() '()) 'h1))
(check-true (has-tag? (element #f #f 'p '() '()) 'p))

;; has-tag?
(check-false (get-tag (pcdata #f #f "a")))
(check-false (get-tag (cdata #f #f "B")))
(check-false (get-tag (attribute #f #f 'a "b")))
(check-equal? (get-tag (element #f #f 'p '() '())) 'p)

;; xml->string
(check-equal? (xml->string (element #f #f 'a '() '())) "<a></a>")
(check-equal?
 (xml->string
  (element #f #f 'a
           (list (attribute #f #f 'one "Two")
                 (attribute #f #f 'three "Four"))
           (list (pcdata #f #f "Hello World!"))))
 "<a one=\"Two\" three=\"Four\">Hello World!</a>")
(check-equal? (xml->string (pcdata #f #f "ABC")) "ABC")
(check-equal? (xml->string (comment "b")) "<!--b-->")


;; xml/list->string
(define c1
  (element #f #f 'a
           (list (attribute #f #f 'one "Two")
                 (attribute #f #f 'three "Four"))
           (list (pcdata #f #f "Hello World!"))))
(check-equal? (xml/list->string (list (element #f #f 'a '() '()))) "<a></a>")
(check-equal? (xml/list->string (list c1))
              "<a one=\"Two\" three=\"Four\">Hello World!</a>")
(check-equal? (xml/list->string (list (pcdata #f #f "ABC"))) "ABC")
(check-equal? (xml/list->string (list (comment "b"))) "<!--b-->")
(check-equal? (xml/list->string (list (comment "Before") c1 (comment "After")))
              "<!--Before--><a one=\"Two\" three=\"Four\">Hello World!</a><!--After-->")

;; string->xml/element
(check-equal? (remove-loc (string->xml/element "<p></p>")) (element #f #f 'p '() '()))
(check-equal?
 (remove-loc (string->xml/element "<a><h1><p></p></h1></a>"))
 (element #f #f 'a '() `(,(element #f #f 'h1 '() `(,(element #f #f 'p '() '()))))))
(check-equal?
 (remove-loc (string->xml/element "<a c=\"b\">stuff</a>"))
 (element #f #f 'a `(,(attribute #f #f 'c "b")) `(,(pcdata #f #f "stuff"))))


;; xml-content
(check-equal? (xml-content (comment "Hello")) (list))
(check-equal? (xml-content (cdata #f #f "Hello")) (list))
(check-equal? (xml-content (pcdata #f #f "Hello")) (list))
(check-equal? (xml-content (entity #f #f 'a)) (list))
(check-equal?
 (xml-content (element #f #f 'a '() (list (pcdata #f #f "Hello"))))
 (list (pcdata #f #f "Hello")))
(check-equal? (xml-content (attribute #f #f 'class "a")) (list (pcdata #f #f "a")))
(check-equal?
 (xml-content
  (document
   (prolog '() #f '())
   (element #f #f 'a '() (list (pcdata #f #f "Hello")))
   (list (comment "the comment"))))
 (list (element #f #f 'a '() (list (pcdata #f #f "Hello")))))
(check-equal?
 (xml-content
  (prolog (list (comment "One")) #f (list (comment "Two"))))
 (list (comment "One") (comment "Two")))

;; content-content
(check-equal? (content-content (comment "Hello")) (list))
(check-equal? (content-content (cdata #f #f "Hello")) (list))
(check-equal? (content-content (pcdata #f #f "Hello")) (list))
(check-equal? (content-content (entity #f #f 'a)) (list))
(check-equal? (content-content (element #f #f 'a '() (list (pcdata #f #f "Hello"))))
              (list (pcdata #f #f "Hello")))
(check-equal?
 (content-content
    (element
     #f #f 'a (list (attribute #f #f 'class "test")) (list (pcdata #f #f "Hello"))))
 (list #;(attribute #f #f 'class "test") (pcdata #f #f "Hello")))

;; content?
(check-true (content? (element #f #f 'a '() (list (pcdata #f #f "Hello")))))
(check-true (content? (pcdata #f #f "Hello")))
(check-true (content? (entity #f #f 'Hello)))
(check-true (content? (comment "Hello")))
(check-true (content? (cdata #f #f "Hello")))
(check-false (content? (attribute #f #f 'a "b")))
(check-false (content? "a"))
