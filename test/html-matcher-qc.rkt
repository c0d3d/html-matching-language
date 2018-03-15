#lang racket
(require "../lib/html-matcher.rkt"
         "../lib/multi-diff-map.rkt"
         "qc-lib.rkt"
         xml
         quickcheck
         rackunit
         rackunit/quickcheck)


(define-chained-gen short-ascii-string-gen
  [len <- (choose-integer 0 4)]
  [s <- (choose-ascii-string len)]
  s)

(define-chained-gen comment-gen
  [str <- short-ascii-string-gen]
  (comment str))

;; For now (locations also don't really matter) ...
(define location-gen (generator-unit #f))

(define-chained-gen start/stop-gen
  [start <- location-gen]
  [stop  <- location-gen]
  (cons start stop))

(define (str-based-tag tfun)
  (bind-chain
   [(cons start stop) <- start/stop-gen]
   [s <- short-ascii-string-gen]
   (tfun start stop s)))

(define-chained-gen attribute-gen
  [(cons start stop) <- start/stop-gen]
  [str <- (choose-ascii-string 1)]
  [v <- (choose-ascii-string 1)]
  (attribute start stop (string->symbol str) v))

(define-chained-gen element-gen
  [(cons start stop) <- start/stop-gen]
  [attrs <- (choose-short-list attribute-gen)]
  [cont <- (choose-short-list content-gen)]
  [tag <- (choose-one-of TAG-LIST)]
  (element start stop tag attrs cont))


(define-chained-gen entity-gen
  [(cons start stop) <- start/stop-gen]
  (entity start stop 'a))

(define cdata-gen (str-based-tag cdata))
(define pcdata-gen (str-based-tag pcdata))
(define prolog-gen (generator-unit (prolog '() #f '())))
(define misc-gen comment-gen)

(define content-gen
  (choose-one-gen
   (list pcdata-gen
         element-gen
         entity-gen
         comment-gen
         cdata-gen)))

(define-chained-gen document-gen
  [p     <- prolog-gen]
  [ele   <- element-gen]
  [len   <- (choose-integer 0 3)]
  [misc  <- (choose-list misc-gen len)]
  (document p ele misc))

(define xml-gen
  (choose-one-gen
   (list document-gen
         content-gen
         prolog-gen
         attribute-gen))) ;; Attribute is (hopefully) going to break a lot of stuff

(define arbitrary-xml (arbitrary xml-gen (λ (x y) y)))

(define TAG-LIST '(a b c d))

(define prop_content-is-content?
  (property ([c content-gen])
            (content? c)))

(define prop_has-tag?
  (property ([x arbitrary-xml]
             [target-tag (choose-one-of TAG-LIST)])
            (define tmp (has-tag? x target-tag))
            (if (and (element? x) (equal? target-tag (element-name x)))
                tmp
                (not tmp))))

(define (found-mapping? loh k v)
  (define (good? h)
    (and (hash-has-key? h k)
         (equal? (hash-ref h k) v)))
  (ormap good? loh))

(define prop_sub-pat-matcher-simple
  (property
   ([x arbitrary-xml]
    [target-tag (choose-one-of TAG-LIST)])
   (define matcher (sub-pat-matcher target-tag 'the-match))
   (==> (has-tag? x target-tag)
        (found-mapping?
         (build-mdm (matcher mdm-empty x))
         'the-match
         (match-data (list target-tag)
                     (xml/list->string (xml-content x)))))))

;; Next:
;; Should contain one match for every instance of a specific tag
;;

(module+ test
  (check-property prop_content-is-content?)
  (check-property prop_has-tag?)
  (with-test-count 100000 (check-property prop_sub-pat-matcher-simple)))
