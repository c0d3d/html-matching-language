#lang racket

(require (for-syntax syntax/parse))
(require "html-matcher.rkt" 
         (prefix-in base: racket))
(provide (for-syntax compile-pattern))

;; Compiles a matching pattern
(define-for-syntax (compile-pattern stx)
  (syntax-parse stx
    [(_ ((~datum quote) a ...))
     #'(base:#%app quote a ...)]
    [(_ name:id) #'name]
    [(_ tag:id inside:expr ...)
     #'(base:#%app
        sub-pat-matcher
        'tag
        inside ...)]))
