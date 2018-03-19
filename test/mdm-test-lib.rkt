#lang racket

(require rackunit
         "../lib/multi-diff-map.rkt"
         racket/hash
         (for-syntax syntax/parse))

(provide
 check-mdm-empty?
 check-builds-to)

(define (check-mdm-empty? x)
  (check-true (mdm-empty? x)))

(define-syntax check-builds-to
  (syntax-parser
    [(_ e:expr (((k v) ...) ...))
    #'(check-equal?
       (build e)
       (list (make-immutable-hash (list (cons k v) ...)) ...))]))
