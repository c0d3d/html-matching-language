#lang racket

(require (for-syntax syntax/parse racket))
(require "../lib/html-matcher.rkt"
         "../lib/multi-diff-map.rkt"
         "../lib/hml-lang.rkt"
         rackunit
         xml)

(provide
 (all-from-out
  racket
  xml
  rackunit
  "../lib/html-matcher.rkt"
  "../lib/multi-diff-map.rkt"
  "../lib/hml-lang.rkt"))

(provide
 test)

(define-syntax test
  (syntax-parser
    [(_ m:expr xexpr ((~seq k:expr (path:expr ...) v:expr) ...) ...)
     #:with scoped-app (datum->syntax #'m '#%app)
     #:with matcher-val #'(let-syntax ([scoped-app compile-pattern]) m)
     #'(check-equal?
        (build (matcher-val mdm-empty (xexpr->xml xexpr)))
        (list (make-immutable-hash (list (cons k (match-data (list 'path ...) v)) ...)) ...))]))
