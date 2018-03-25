#lang racket

(require (for-syntax syntax/parse)
         syntax/parse)

(provide ~kw-with-default
         kw-arg)

;; Shorthand for a keyword arg and val pair
(define-splicing-syntax-class kw-arg
  (pattern (~and (~seq _:keyword _:expr)
                 (~seq pair ...))))

(define-syntax ~kw-with-default
  (pattern-expander
   (syntax-parser
     [(_ kw name default)
      ;; This is terrible, and probably shouldn't be necessary if I was doing this properly
      ;; too bad I don't know what "properly" is :P .
      (datum->syntax
       #'kw
       (syntax->datum #'(~optional (~seq (~datum kw) name) #:defaults ([name default]))))])))
