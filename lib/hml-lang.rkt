#lang racket

(require (for-syntax syntax/parse racket/syntax))
(require "html-matcher.rkt"
         "multi-diff-map.rkt"
         (prefix-in base: racket)
         xml)
(provide
 (for-syntax make-pattern*)
 make-pattern
 match/html
 string->xml/element
 build-mdm
 (except-out (all-from-out xml "html-matcher.rkt") attribute))


;; Compiles a pattern
(define-for-syntax (make-pattern* stx)
  (define (compile-pattern stx)
    (syntax-parse stx
      [(_ ((~datum quote) a:id))
       #'(base:#%app quote a)]
      [(_ tag:id inside:expr ...)
       #'(base:#%app
          ordered-sub-pat-matcher
          'tag
          inside ...)]))
  (define app-name (format-id stx "#%app"))
  (syntax-parse stx
    [arg:expr
     #`(let-syntax ([#,app-name #,compile-pattern]) arg)]))

;; Syntax version of the compile time function
(define-syntax (make-pattern stx)
  (syntax-parse stx
    [(_ a)
     (make-pattern* #'a)]))

(define-syntax (match/html stx)
  (syntax-parse stx
    [(_ pat:id doc:expr body:expr)
     #:with mm-name (format-id stx "mm")
     #`(for/list ([mm-name (build-mdm (pat mdm-empty doc))])
         body)]
    [(_ pat:expr doc:expr body:expr)
     #:with mm-name (format-id stx "mm")
     #`(for/list ([mm-name (build-mdm ((make-pattern pat) mdm-empty doc))])
         body)]))
