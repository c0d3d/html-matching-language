#lang racket

(require xml "multi-diff-map.rkt")

(module+ test (require rackunit racket/match))


;; A Xml is one of:
;;  - Document
;;  - Content
;;  - Prolog
;;  - Attribute

;; A Document is a (document Prolog Element [Listof Misc])
;; names:                    prolog element misc
;; This is the top level value for an xml.

;; A Content is one of:
;;  - PCData
;;  - Element
;;  - Entity
;;  - Comment
;;  - CData

;; A Prolog is a (prolog [Listof Misc] Dtd [Listof Misc])
;; names:                misc          dtd misc2

;; An Element is a (element Location Location Symbol [Listof Attribute] [Listof Content])
;; names:                   start    stop     name   attributes         content

;; A Misc one of
;;  - ProcInstr
;;  - Comment

;; A Location is one of:
;;  - Boolean
;;  - (location Number Number Number)
;;              line   char   offset

;; An Attribute is a (attribute Location Location Symbol String)
;; names:                       start    stop     name   value

;; A Comment is a (comment Location Location String)
;; names:                  start    stop     text

;; An Entity is an (entity Location Location [Or Symbol Char])
;; names:                  start    stop     text

;; A PCData is a (pcdata Location Location String)
;; names:                start    stop     string

;; A CData is a (cdata Location Location String)
;; names:              start    stop     string

;; A Dtd is a type of value in the xml module that we do not support.

;; A MatchMap is a MDiffMap
;; with all pattern variables associated to their respective values.

;; a MatchFailed is a continuation that should be called when a match fails.

;; A Matcher has the signature (MatchFailed Doc -> MatchMap)
;; These are how you can match a specific piece of HTML

;; an XmlPred is a (Xml -> Boolean)

(define (doc-from-file path)
  (call-with-input-file path read-xml))


; Note: these are missing the unsupported
; xml type (p-i)
(define CONTENT_PREDS `(,element? ,entity? ,comment? ,pcdata? ,cdata?))

;; Any -> Boolean
;; Is the given value a Comment?
(define (content? x)
  (for/or ([f CONTENT_PREDS])
    (f x)))
(module+ test
  (check-true (content? (element #f #f 'a '() (list (pcdata #f #f "Hello")))))
  (check-true (content? (pcdata #f #f "Hello")))
  (check-true (content? (entity #f #f 'Hello)))
  (check-true (content? (comment "Hello")))
  (check-true (content? (cdata #f #f "Hello")))
  (check-false (content? (attribute #f #f 'a "b")))
  (check-false (content? "a")))

;; Content -> [Listof Xml]
;; Extracts the given Content's content.
(define (content-content xpr)
  (define is-simple?
    (or (cdata? xpr) (pcdata? xpr) (entity? xpr) (comment? xpr)))
  (cond
    [is-simple? (list xpr)]
    [(element? xpr) (element-content xpr)]))
(module+ test
  (check-equal? (content-content (comment "Hello")) (list (comment "Hello")))
  (check-equal? (content-content (cdata #f #f "Hello")) (list (cdata #f #f "Hello")))
  (check-equal? (content-content (pcdata #f #f "Hello")) (list (pcdata #f #f "Hello")))
  (check-equal? (content-content (entity #f #f 'a)) (list (entity #f #f 'a)))
  (check-equal? (content-content (element #f #f 'a '() (list (pcdata #f #f "Hello"))))
                (list (pcdata #f #f "Hello"))))

;; Xml -> [Listof Xml]
;; Extracts the content of a given Xml value.
;; Note that the "content" for each kind of element is defined
;; in our design document.
(define (xml-content xpr)
  (cond
    [(document? xpr)
     (cons (document-element xpr)
           (append-map xml-content (document-misc xpr)))]
    [(content? xpr) (content-content xpr)]
    [(prolog? xpr) (append (prolog-misc xpr) (prolog-misc2 xpr))]
    [(attribute? xpr) (list (attribute-value xpr))]))
(module+ test
  (check-equal? (xml-content (comment "Hello")) (list (comment "Hello")))
  (check-equal? (xml-content (cdata #f #f "Hello")) (list (cdata #f #f "Hello")))
  (check-equal? (xml-content (pcdata #f #f "Hello")) (list (pcdata #f #f "Hello")))
  (check-equal? (xml-content (entity #f #f 'a)) (list (entity #f #f 'a)))
  (check-equal? (xml-content (element #f #f 'a '() (list (pcdata #f #f "Hello"))))
                (list (pcdata #f #f "Hello")))
  (check-equal? (xml-content (attribute #f #f 'class "a")) '("a"))
  (check-equal? (xml-content
                 (document
                  (prolog '() #f '())
                  (element #f #f 'a '() (list (pcdata #f #f "Hello")))
                  (list (comment "the comment"))))
                 (list (element #f #f 'a '() (list (pcdata #f #f "Hello")))
                       (comment "the comment")))
  (check-equal? (xml-content
                 (prolog (list (comment "One")) #f (list (comment "Two"))))
                (list (comment "One") (comment "Two"))))

;; XExprPred Symbol -> Matcher
;; Creates a matcher that uses the given element predicate to
;; match elements, and uses the given symbol inside the output match
#;(define (mk-matcher cmp name)
  (λ (escape-hatch doc)
    (if (cmp doc)

        )))

; (xml->xexpr (document-element (read-xml (open-input-string "<document><p></p></document>"))))
