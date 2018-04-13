#lang racket

(require racket/generic)

(provide gen:matcher
         matcher?
         attempt-match
         get-consume-range)


(define-generics matcher
  ; Matcher MatchState -> (Or False MatchState)
  [attempt-match matcher acc]
  ; Matcher -> (Cons (Or Nat #f) (Or Nat #f))
  ; Provides the range of Nat's that are valid to consume
  ; for the given matcher
  ; Range is [low high)
  ; #f indicates no limit on that side of the interval.
  [get-consume-range matcher])
