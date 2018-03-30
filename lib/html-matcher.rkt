#lang racket

(require (for-syntax racket/syntax "html-matcher-syntax.rkt" syntax/parse)
         xml
         racket/splicing
         "multi-diff-map.rkt")

(provide
 ordered-sub-pat-matcher
 top-level-anchored-matcher
 content?
 has-tag?
 sub-pat-matcher
 xml/list->string
 xml-content
 match-data
 string->xml/element
 xexpr->xml/element
 get-tag
 xml->string
 content-content
 splicing-let
 ms-empty
 build-ms)

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
(struct match-state (remain mdm))

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
  (define (folder nxt acc)
    (ms-add-child acc (matcher ms-empty nxt)))
  (foldl folder parent xmls))

;; From: https://docs.racket-lang.org/reference/contmarks.html
(define (extract-current-continuation-marks key)
  (continuation-mark-set->list
   (current-continuation-marks)
   key))


(define-for-syntax (fixup-syntax-list l)
  (cond
    [(and (list? l) (pair? l)) #`(cons #,(car l) #,(fixup-syntax-list (cdr l)))]
    [(and (pair? l) (pair? (cdr l)))
     #`(cons #,(car l) #,(fixup-syntax-list (cdr l)))]
    [(pair? l)
     #`(cons #,(car l) #,(cdr l))]
    [else #'null]))

(define TOP-LEVEL-CM 'top-level)
(define TAG-CM 'tag)
(define-syntax (define-match-maker stx)
  (syntax-parse stx
    [(_ (~kw-with-default #:acc-name acc-name (format-id stx "acc"))
        (~kw-with-default #:disable-self-app dsa #'#f)
        (fname:id . args)
        [((~datum nest-tag) tname nxt-xmls)
         the-kw-args1:kw-arg ...
         body1 ...] ...
        [((~datum leaf-tag) dname)
         the-kw-args2:kw-arg ...
         body2 ...] ...)
     (define +args (if (pair? (syntax->datum #'args)) (syntax-e #'args) (list #'args)))
     (define argsl (fixup-syntax-list +args))
     #`(define (fname #,@+args)
         (define (the-match #,@+args)
           (define (self)
             (λ (acc-name cur-lvl)
               (define self-app? (should-self-apply?))
               (match cur-lvl
                 [(nest-tag tname nxt-xmls)
                  the-kw-args1.pair ... ...
                  (with-continuation-mark TAG-CM tname
                    (let ([the-ans
                           ; Always apply the top-level mark, will duplicate but whatever
                           (with-continuation-mark TOP-LEVEL-CM #t
                             ((λ () body1 ...)))])
                      (if (and (not dsa) self-app?)
                          (match/foldl (self) the-ans (filter nest-tag? (map as-lvl nxt-xmls)))
                          the-ans)))] ...
                 [(leaf-tag dname)
                  the-kw-args2.pair ... ...
                  body2 ...] ...)))
           (self))
         (define the-fun (apply the-match #,argsl))
         (λ (acc next-xmls)
           (the-fun acc (as-lvl next-xmls))))]))


(define (should-self-apply?)
  (empty? (extract-current-continuation-marks TOP-LEVEL-CM)))

(define (as-lvl xml)
  (define t (get-tag xml))
  (if t (nest-tag t (xml-content xml)) (leaf-tag xml)))

(define (add-or-perform-sub content-matcher acc remain #:as-child [as-child? #t])
  (if (symbol? content-matcher)
      (set-match acc content-matcher remain #:as-child as-child?)
      (match/foldl content-matcher acc remain)))

(define-match-maker (ordered-sub-pat-matcher ele . subs)
  [(nest-tag tag-name sub-xmls)
   #:when (and (equal? (length sub-xmls) (length subs))
               (eq? ele tag-name))
   (let/cc bail
     (define (fail) (bail acc))
     (define (fold-one nxt-fun nxt-xml acc)
       (cond
         [(symbol? nxt-fun)
          (set-match acc nxt-fun nxt-xml)]
         [else
          (define the-ans (nxt-fun ms-empty nxt-xml))
          (if (ms-empty? the-ans)
              (fail)
              (foldl ms-join acc (ms-get-children the-ans)))]))
     (foldl fold-one acc subs sub-xmls))]
  [(nest-tag tag-name sub-xmls) acc]
  [(leaf-tag tag-data) acc])


(define-match-maker (sub-pat-matcher ele sub)
  [(nest-tag tag-name sub-xmls)
   #:when (symbol=? tag-name ele)
   (add-or-perform-sub sub acc sub-xmls)]
  ; Current limitation: This needs to be here to continue the traversal.
  [(nest-tag a b) acc]
  ; This matcher only matches tags.
  ; So we never match just data
  [(leaf-tag tag-data) acc])

;; This one doesn't self apply
;; So it will only continue if it matches starting at the top level.
(define-match-maker
  #:disable-self-app #t
  (top-level-anchored-matcher ele sub)
  [(nest-tag name sub-xmls)
   #:when (not ele) ; an "always match"
   (set-match acc sub sub-xmls)]
  [(nest-tag name sub-xmls) ; a "match a tag"
   #:when (and (procedure? sub)
               (symbol=? name ele))
   (match/foldl sub acc sub-xmls)]
  [(nest-tag name sub-xmls) ; a "match a tag"
   #:when (and (symbol? sub)
               (symbol=? name ele))
   (set-match acc sub sub-xmls)]
  [(nest-tag name sub-xmls) acc]
  [(leaf-tag dat)
   #:when (not ele)
   (set-match acc sub dat)]
  [(leaf-tag dat) acc])

(define (set-match ms name dat #:as-child [as-child? #t])
  (define path (reverse (extract-current-continuation-marks TAG-CM)))
  (define dat-as-string
    (if (list? dat) (xml/list->string dat) (xml->string dat)))
  (if as-child?
      (ms-add-child
       ms
       (ms-of name (match-data path dat-as-string)))
      (ms-set ms name dat)))

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

(define ms-empty (match-state '() mdm-empty))
(define/contract ms-empty?
  (-> match-state? boolean?)
  (compose mdm-empty? match-state-mdm))
(define/contract (ms-get-children ms)
  (-> match-state? (listof match-state?))
  (map
   (λ (x) (match-state '() x))
   ((compose mdm-get-children match-state-mdm) ms)))
(define/contract (ms-join ms1 ms2)
  (-> match-state? match-state? match-state?)
  (match-define (match-state r1 m1) ms1)
  (match-define (match-state r2 m2) ms2)
  (match-state
   (append r1 r2)
   (mdm-join m1 m2)))
(define/contract (ms-add-child ms c)
  (-> match-state? match-state? match-state?)
  (match-state
   (match-state-remain ms)
   (mdm-add-child (match-state-mdm ms) (match-state-mdm c))))
(define/contract (ms-set ms k v)
  (-> match-state? any/c any/c match-state?)
  (match-define (match-state r m) ms)
  (match-state r (mdm-set m k v)))
(define/contract (ms-of k v)
  (-> any/c any/c match-state?)
  (match-state '() (mdm-with [k v])))
(define build-ms (compose build-mdm match-state-mdm))
