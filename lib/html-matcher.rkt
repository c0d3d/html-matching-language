#lang racket

(require (for-syntax syntax/parse)
         xml
         "multi-diff-map.rkt")

(provide
 top-level-anchored-matcher
 content?
 has-tag?
 sub-pat-matcher
 xml/list->string
 xml-content
 match-data
 string->xml/element
 build
 get-tag
 xml->string
 content-content)

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
  (call-with-input-file path read-xml))

(struct match-data (path text) #:transparent)
(struct nest-tag (sym contents) #:transparent)
(struct leaf-tag (data) #:transparent)

; Note: these are missing the unsupported
; xml type (p-i)
(define CONTENT_PREDS `(,element? ,entity? ,comment? ,pcdata? ,cdata?))

;; Any -> Boolean
;; Is the given value a Comment?
(define (content? x)
  (for/or ([f CONTENT_PREDS])
    (f x)))


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


(define build (compose remove-duplicates build-mdm))

;; Xml Symbol -> Bool
;; Returns true if the given Xml is an element with the given tag.
(define (has-tag? xml tag)
  (and (element? xml) (eq? tag (element-name xml))))


;; Xml -> [Or Symbol False]
;; Gets the tag for the given xml if it is an element,
;; otherwise returns #f
(define (get-tag xml)
  (and (element? xml) (element-name xml)))


;; Matcher MDiffMap [Listof Xml] -> MDiffMap
;; Folds the given matcher over the given list of xml's
;; if any provide a match
(define (match/foldl matcher parent xmls)
  (foldl (λ (nxt acc) (mdm-add-child acc (matcher mdm-empty nxt))) parent xmls))

;; From: https://docs.racket-lang.org/reference/contmarks.html
(define (extract-current-continuation-marks key)
  (continuation-mark-set->list
   (current-continuation-marks)
   key))

(define-syntax define-match-maker
  (syntax-parser
    [(_ acc-name:id
        (fname:id args:id ...)
        [((~datum nest-tag) name-name:id content-name:id)
         (~optional (~seq (~datum #:apply-self) (~bind [app-self #'#t]))
                    #:defaults ([app-self #'#f]))
         (~optional (~and (~seq kw:keyword kv:expr)
                          (~seq kw-args ...))
                    #:defaults ([kw #'#f] [kv #'#f] [(kw-args 1) (list #'(void))]))
         inside1 ...] ...
        [((~datum leaf-tag) data-name:id)
         (~optional (~and (~seq leaf-kw:keyword leaf-kv:expr)
                          (~seq leaf-kw-args ...))
                    #:defaults ([leaf-kw #'#f] [leaf-kv #'#f] [(leaf-kw-args 1) (list #'(void))]))
         inside2 ...] ...)
     #'(define (fname args ...)
         (define (match-nest-tag args ...)
           (define (self)
             (λ (acc-name cur-lvl)
               (match cur-lvl
                 [(nest-tag name-name content-name)
                  kw-args ...
                  (with-continuation-mark 'tag name-name
                    (let ([ans1 ((λ () inside1 ...))]) ;; Allow for definition context
                      (if app-self
                          (match/foldl (self) ans1 (filter nest-tag? (map as-lvl content-name)))
                          ans1)))] ...
                 [(leaf-tag data-name)
                  leaf-kw-args ...
                  inside2 ...] ...)))
           (self))
         (define the-fun (match-nest-tag args ...))
         (λ (acc next-xmls) (the-fun acc (as-lvl next-xmls))))]))

(define (as-lvl xml)
  (define t (get-tag xml))
  (if t (nest-tag t (xml-content xml)) (leaf-tag xml)))

(define (add-or-perform-sub content-matcher acc remain)
  (if (symbol? content-matcher)
      (set-match acc content-matcher remain)
      (match/foldl content-matcher acc remain)))

(define-match-maker acc (sub-pat-matcher ele sub)
  [(nest-tag tag-name sub-xmls)
   #:apply-self
   #:when (symbol=? tag-name ele)
   (add-or-perform-sub sub acc sub-xmls)]
  ; Current limitation: This needs to be here to continue the traversal.
  [(nest-tag a b) #:apply-self acc]
  ; This matcher only matches tags.
  ; So we never match just data
  [(leaf-tag tag-data) acc])

;; This one doesn't self apply
;; So it will only continue if it matches starting at the top level.
(define-match-maker acc (top-level-anchored-matcher ele sub)
  [(nest-tag name sub-xmls)
   #:when (not ele) ; an "always match"
   (set-match acc sub sub-xmls)]
  [(nest-tag name sub-xmls) ; a "match a tag"
   #:when (and (procedure? sub)
               (symbol=? name ele))
   (match/foldl sub acc sub-xmls)]
  [(nest-tag name sub-xmls) ; a "match a tag"
   #:when (symbol=? name ele)
   (set-match acc sub sub-xmls)]
  [(nest-tag name sub-xmls) acc]
  [(leaf-tag dat)
   #:when (not ele)
   (set-match acc sub dat)]
  [(leaf-tag dat) acc])

(define (set-match mdm name dat)
  (define path (reverse (extract-current-continuation-marks 'tag)))
  (define dat-as-string
    (if (list? dat) (xml/list->string dat) (xml->string dat)))
  (mdm-add-child mdm (mdm-with
                      [name (match-data path dat-as-string)])))

;; Xml -> String
;; Converter from xml to string repr
(define xml->string (compose xexpr->string xml->xexpr))


;; [Listof Xml] -> String
;; Converts each xml value into a string
;; and concats them all together in the list order
(define (xml/list->string l)
  (foldl (λ (nxt acc) (string-append acc (xml->string nxt))) "" l))


;; String -> Xml
;; Converter from strings to xml repr
(define string->xml/element
  (compose document-element read-xml/document open-input-string))
