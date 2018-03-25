#lang racket

(require (for-syntax syntax/parse
                     racket
                     "../lib/hml-lang.rkt"))
(require "../lib/multi-diff-map.rkt"
         "../lib/hml-lang.rkt"
         rackunit)

(provide
 (all-from-out
  racket
  rackunit
  "../lib/hml-lang.rkt"))

(provide
 test)

(define-syntax (test stx)
  (syntax-parse stx
    [(_ m:expr xexpr ((~seq k:expr (path:expr ...) v:expr) ...) ...)
     #`(check-equal?
        (build-mdm (#,(make-pattern* #'m) mdm-empty (xexpr->xml xexpr)))
        (list (make-immutable-hash (list (cons k (match-data (list 'path ...) v)) ...)) ...))]))
