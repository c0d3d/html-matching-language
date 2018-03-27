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
    [(_ m:expr xml ((~seq k:expr (path:expr ...) v:expr) ...) ...)
     #'(check-equal?
        (match/html m (xexpr->xml xml) mm)
        (list (make-immutable-hash (list (cons k (match-data (list 'path ...) v)) ...)) ...))]))
