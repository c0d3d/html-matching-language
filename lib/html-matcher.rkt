#lang racket

(require (for-syntax racket/syntax "html-matcher-syntax.rkt" syntax/parse)
         xml
         racket/splicing
         "multi-diff-map.rkt")

(provide

 content/c
 content?

 has-tag?
 xml/list->string
 xml-content
 match-data
 string->xml/element
 xexpr->xml/element
 get-tag
 xml->string
 content-content
 splicing-let
 doc-from-file
 decompose-element)

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

;; A Comment is a (comment String)
;; names:                  text

;; An Entity is an (entity Location Location [Or Symbol Char])
;; names:                  start    stop     text

;; A PCData is a (pcdata Location Location String)
;; names:                start    stop     string

;; A CData is a (cdata Location Location String)
;; names:              start    stop     string

;; A Dtd is a type of value in the xml module that we do not support.

;; A MatchLvl is one of:
;; - (nest-tag name sub-eles)
;; - (leaf-tag data)

;; A Matcher has the signature (MatchMap MatchLvl -> MatchMap)
;; These are functions which will compute matches. The function should
;; assign matches into the given MatchMap based on its matching criteria,
;; as well as the kind of MatchLvl that was passed in.

;; A MatchMap is a MDiffMap
;; with all pattern variables associated to their respective values.

;; A Matcher has the signature (MatchMap Xml -> MatchMap)
;; These are how you can match a specific piece of HTML
;; The work with the given MatchMap which is the accumulation of the processed data.

;; A ContentMatcher is one of:
;; - Symbol
;; - Matcher

;; an XmlPred is a (Xml -> Boolean)

(define (doc-from-file path)
  (document-element (call-with-input-file path read-xml)))

(struct match-data (path text) #:transparent)
(struct nest-tag (sym contents) #:transparent)
(struct leaf-tag (data) #:transparent)
(struct match-state (remain mdm))

; Note: these are missing the unsupported
; xml type (p-i)
(define CONTENT_PREDS `(,element? ,entity? ,comment? ,pcdata? ,cdata?))

;; Any -> Boolean
;; Is the given value a Comment?
(define (content? x)
  (for/or ([f CONTENT_PREDS])
    (f x)))

(define/contract (decompose-element e)
  (-> element? (values symbol? (listof content/c)))
  (values (get-tag e) (xml-content e)))


;; Content -> [Listof Xml]
;; Extracts the given Content's content.
(define (content-content xpr)
  (define is-simple?
    (or (cdata? xpr) (pcdata? xpr) (entity? xpr) (comment? xpr)))
  (cond
    [is-simple? (list)]
    [(element? xpr)
     (append '() #;(TODO element-attributes xpr) (element-content xpr))]))


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
    [(attribute? xpr) (list (pcdata (source-start xpr) (source-stop xpr)
                                    (attribute-value xpr)))]))



;; Xml Symbol -> Bool
;; Returns true if the given Xml is an element with the given tag.
(define (has-tag? xml tag)
  (and (element? xml) (eq? tag (element-name xml))))


;; Xml -> [Or Symbol False]
;; Gets the tag for the given xml if it is an element,
;; otherwise returns #f
(define (get-tag xml)
  (and (element? xml) (element-name xml)))


;; Xml -> String
;; Converter from xml to string repr
(define xml->string (compose xexpr->string xml->xexpr))

;; XExpr -> Xml
;; Convert from XExpr to Xml element.
(define xexpr->xml/element (compose document-element xexpr->xml))


;; [Listof Xml] -> String
;; Converts each xml value into a string
;; and concats them all together in the list order
(define (xml/list->string l)
  (foldl (λ (nxt acc) (string-append acc (xml->string nxt))) "" l))


;; String -> Xml
;; Converter from strings to xml repr
(define string->xml/element
  (compose document-element read-xml/document open-input-string))
