#lang racket

(require (for-syntax syntax/parse racket/syntax racket (only-in "matcher-lib.rkt" WILDCARD-SYM)))
(require (for-template ))
(require "html-matcher.rkt"
         "matcher-lib.rkt"
         "multi-diff-map.rkt"
         (prefix-in base: racket)
         xml)
(provide
 (for-syntax make-pattern*)
 make-pattern
 match/html
 string->xml/element
 (except-out (all-from-out xml "html-matcher.rkt") attribute))

(define-for-syntax (select-matcher-for data)
  (cond
    [(ormap (λ (x . y) (eq? '* x)) data) #'(λ (x) x)]
    [else #'simple-tag-matcher]))

(define-for-syntax (parse-pattern stx)
  (syntax-parse stx
    [(~datum *) #'WILDCARD-SYM]
    [((~datum quote) x) #'(data-matcher 'x)]
    [(tag-name:id ((key:id val:expr) ...) contents:expr ...)
     #:with matcher (select-matcher-for (syntax->datum stx))
     #`(matcher 'tag-name
                ((key val) ...)
                #,@(map parse-pattern (syntax->list #'(contents ...))))]
    [(tag-name:id contents:expr ...)
     #:with matcher (select-matcher-for (syntax->datum stx))
     #`(matcher 'tag-name
                #,@(map parse-pattern (syntax->list #'(contents ...))))]
    [s (error (syntax->datum #'s))]))

;; Compiles a pattern
(define-for-syntax (make-pattern* stx)
  (parse-pattern stx))

;; Syntax version of the compile time function
(define-syntax (make-pattern stx)
  (syntax-parse stx
    [(_ a)
     (make-pattern* #'a)]))

(define-syntax (match/html stx)
  (syntax-parse stx
    [(_ pat:id doc:expr body:expr)
     #:with mm-name (format-id stx "mm")
     #`(for/list ([mm-name (build-ms (apply-to-completion pat (list doc)))])
         body)]
    [(_ pat:expr doc:expr body:expr)
     #:with mm-name (format-id stx "mm")
     #`(for/list ([mm-name (build-ms (apply-to-completion (make-pattern pat) (list doc)))])
         body)]))
