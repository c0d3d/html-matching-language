#lang racket

(require xml "multi-diff-map.rkt" racket/hash)
(require (for-syntax syntax/parse))

(module+ test
  (require rackunit racket/match "../test/mdm-test-lib.rkt")
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
      [else x])))
(provide
 content?
 has-tag?
 sub-pat-matcher
 xml/list->string
 xml-content
 match-data
 string->xml/element
 build)

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
    [is-simple? (list)]
    [(element? xpr)
     (append '() #;(TODO element-attributes xpr) (element-content xpr))]))
(module+ test
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
   (list #;(attribute #f #f 'class "test") (pcdata #f #f "Hello"))))

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
(module+ test
  (check-equal? (xml-content (comment "Hello")) (list))
  (check-equal? (xml-content (cdata #f #f "Hello")) (list))
  (check-equal? (xml-content (pcdata #f #f "Hello")) (list))
  (check-equal? (xml-content (entity #f #f 'a)) (list))
  (check-equal? (xml-content (element #f #f 'a '() (list (pcdata #f #f "Hello"))))
                (list (pcdata #f #f "Hello")))
  (check-equal? (xml-content (attribute #f #f 'class "a")) (list (pcdata #f #f "a")))
  (check-equal? (xml-content
                 (document
                  (prolog '() #f '())
                  (element #f #f 'a '() (list (pcdata #f #f "Hello")))
                  (list (comment "the comment"))))
                 (list (element #f #f 'a '() (list (pcdata #f #f "Hello")))))
  (check-equal? (xml-content
                 (prolog (list (comment "One")) #f (list (comment "Two"))))
                (list (comment "One") (comment "Two"))))

;; Syntax extension for making
;; functions that produce matchers
(define-syntax define-matcher-maker
  (syntax-parser
    [(_ (fun-name:id fun-args:id ...) body ...)
     #:with acc-name (datum->syntax #'fun-name 'acc)
     #:with doc-name (datum->syntax #'fun-name 'doc)
     #'(define (fun-name fun-args ...)
         (λ (acc-name doc-name)
           body ...))]
    [(_ (fun-name:id [rec-name:id] fun-args:id ...) body ...)
     #:with acc-name (datum->syntax #'fun-name 'acc)
     #:with doc-name (datum->syntax #'fun-name 'doc)
     #'(define (fun-name fun-args ...)
         (letrec ([rec-name
                   (λ (acc-name doc-name)
                     body ...)])
           rec-name))]))

(define (build mdm)
  ;; (displayln (format "mdm:build ~a" (build-mdm mdm)))
  ;; (define into-hash
  ;;   (foldl (λ (nxt acc)
  ;;            (foldl
  ;;             (λ (nxt2 acc2)
  ;;               (hash-set acc2 (cons (rest nxt2) )))
  ;;             acc
  ;;             (hash->list nxt)))
  ;;          (make-immutable-hash)
  ;;          (build-mdm mdm)))
  ;; (displayln (format "into ~a" into-hash))
  ;; (define out-of-hash (hash->list into-hash))
  ;; (displayln (format "out-of-hash ~a" out-of-hash))
  ;; (define condensed (map (λ (p)
  ;;                          (map (λ (x) (make-immutable-hash
  ;;                                       (list (cons (first p) x))))
  ;;                               (second p)))
  ;;                        out-of-hash))
  ;; (displayln (format "condensed ~a" condensed))
  ;; (define joined (apply (λ (a . x) (apply map (cons list (cons a x)))) condensed))
  ;; (displayln (format "joined ~a" joined))
  ;; (define folded (map (λ (x) (foldl hash-union (make-immutable-hash) x)) joined))
  (remove-duplicates (build-mdm mdm)))

;; Xml Symbol -> Bool
;; Returns true if the given Xml is an element with the given tag.
(define (has-tag? xml tag)
  (and (element? xml) (eq? tag (element-name xml))))
(module+ test
  (check-false (has-tag? (pcdata #f #f "a") 'a))
  (check-false (has-tag? (cdata #f #f "B") 'B))
  (check-false (has-tag? (attribute #f #f 'a "b") 'a))
  (check-false (has-tag? (element #f #f 'p '() '()) 'h1))
  (check-true (has-tag? (element #f #f 'p '() '()) 'p)))

;; Xml -> [Or Symbol False]
;; Gets the tag for the given xml if it is an element,
;; otherwise returns #f
(define (get-tag xml)
  (and (element? xml) (element-name xml)))
(module+ test
  (check-false (get-tag (pcdata #f #f "a")))
  (check-false (get-tag (cdata #f #f "B")))
  (check-false (get-tag (attribute #f #f 'a "b")))
  (check-equal? (get-tag (element #f #f 'p '() '())) 'p))

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

;; Symbol ContentMatcher -> Matcher
;; Creates a matcher that matches only nesting tags that have the given name.
;; if a passed in doc has the correct name, the contents are passed into the
;; content-matcher, and the result of that matcher becomes the result of this
;; matcher.
(define-matcher-maker (sub-pat-matcher [self-fun] ele-tag content-matcher)
  (define this-lvl-matches? (has-tag? doc ele-tag))
  (define cur-tag (get-tag doc))
  (with-continuation-mark 'tag cur-tag
    (let ([this-lvl-mm
           (cond
             [(not this-lvl-matches?) acc]
             ; Now we know that (equal? this-lvl-matches? true)
             [(symbol? content-matcher)
              (define cur-marks (reverse (extract-current-continuation-marks 'tag)))
              (define next-match (match-data cur-marks (xml/list->string (xml-content doc))))
              (mdm-add-child acc (mdm-with [content-matcher next-match]))]
             [else
              (match/foldl content-matcher acc (xml-content doc))])])
      (match/foldl self-fun this-lvl-mm (xml-content doc)))))
(module+ test
  ; Matches "a" tags, and binds their content the key 'content
  (define a-pat-matcher (sub-pat-matcher 'a 'content))
  (check-mdm-empty? (a-pat-matcher mdm-empty (comment "a")))
  (check-mdm-empty? (a-pat-matcher mdm-empty (pcdata #f #f "a")))
  (check-mdm-empty? (a-pat-matcher mdm-empty (cdata #f #f "a")))
  (check-mdm-empty? (a-pat-matcher mdm-empty (element #f #f 'b '() '()))))

;; Xml -> String
;; Converter from xml to string repr
(define xml->string (compose xexpr->string xml->xexpr))
(module+ test
  (check-equal? (xml->string (element #f #f 'a '() '())) "<a></a>")
  (check-equal? (xml->string (element #f #f 'a
                                      (list (attribute #f #f 'one "Two")
                                            (attribute #f #f 'three "Four"))
                                      (list (pcdata #f #f "Hello World!"))))
                "<a one=\"Two\" three=\"Four\">Hello World!</a>")
  (check-equal? (xml->string (pcdata #f #f "ABC")) "ABC")
  (check-equal? (xml->string (comment "b")) "<!--b-->"))

;; [Listof Xml] -> String
;; Converts each xml value into a string
;; and concats them all together in the list order
(define (xml/list->string l)
  (foldl (λ (nxt acc) (string-append acc (xml->string nxt))) "" l))
(module+ test
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
                "<!--Before--><a one=\"Two\" three=\"Four\">Hello World!</a><!--After-->"))

;; String -> Xml
;; Converter from strings to xml repr
(define string->xml/element
  (compose document-element read-xml/document open-input-string))
(module+ test
  (check-equal? (remove-loc (string->xml/element "<p></p>")) (element #f #f 'p '() '()))
  (check-equal?
   (remove-loc (string->xml/element "<a><h1><p></p></h1></a>"))
   (element #f #f 'a '() `(,(element #f #f 'h1 '() `(,(element #f #f 'p '() '()))))))
  (check-equal?
   (remove-loc (string->xml/element "<a c=\"b\">stuff</a>"))
   (element #f #f 'a `(,(attribute #f #f 'c "b")) `(,(pcdata #f #f "stuff")))))



;; Syntax to calls the given function with the given arguments
;; except a MatchFailed is inserted in front of
;; the first given argument.
(define-syntax call/escape
  (syntax-parser
    [(_ f args ...)
     #'(let/ec ec (f (λ ([v #f]) (ec v)) args ...))]))
