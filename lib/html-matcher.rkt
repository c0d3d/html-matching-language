#lang racket

(module+ test (require rackunit))

;; A Doc is a piece of HTML

;; A MatchMap is an immutable hash
;; with all pattern variables associated to their respective values.

;; a MatchFailed is a continuation that should be called when a match fails.

;; A Matcher has the signature (MatchFailed Doc -> MatchMap)
;; These are how you can match a specific piece of HTML
