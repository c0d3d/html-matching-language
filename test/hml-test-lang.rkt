#lang racket

(require (for-syntax syntax/parse
                     racket
                     "../lib/hml-lang.rkt")
         xml)
(require "../lib/hml-lang.rkt"
         rackunit)

(provide
 (except-out (all-from-out racket
                           rackunit
                           "../lib/hml-lang.rkt"
                           xml)
             attribute))

(provide test)

(define-syntax (test stx)
  (syntax-parse stx
    [(_ m:expr xml ((~seq k:expr (path:expr ...) v:expr) ...) ...)
     (syntax/loc stx
       (check-equal?
        (match/html m (xexpr->xml xml) mm)
        (list (make-immutable-hash
               (list (cons k (match-data (list 'path ...) v)) ...)) ...)))]))
