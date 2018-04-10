#lang racket

(require (for-syntax syntax/parse
                     syntax/quote
                     ))
(require racket/generic "html-matcher.rkt" "multi-diff-map.rkt" "match-state.rkt" xml
         (prefix-in base: racket))

(provide
 attempt-match
 simple-tag-matcher
 data-matcher
 ms-empty
 build-ms
 apply-to-completion
 WILDCARD-SYM)

(define WILDCARD-SYM (gensym 'wildcard))

(define bail (make-parameter #f))
(define (bail-out x)
  ((bail) x))
(define-syntax bail-to-fail
  (syntax-parser
    [(_ x ...)
     #'(let/cc k
         (parameterize ([bail k])
           x ...))]))

#;(define (merge-state-results s1 s2)
  (and s1 s2 (ms-join s1 s2)))

(define-generics matcher
  ; Matcher MatchState -> (Or False MatchState)
  [attempt-match . (matcher acc)])

(define/contract (apply-matchers matchers state #:strict [is-strict? #f])
  (->* ((listof matcher?) match-state?) (#:strict boolean?) (or/c match-state? false?))
  (define/contract (apply-single* matcher cur-state)
    (-> matcher? (or/c match-state? false?) (or/c false? match-state?))
    (and cur-state (bail-to-fail (attempt-match matcher cur-state))))
  (define (apply-single matcher acc)
    (define ans (apply-single* matcher acc))
    (or ans
        (and (not is-strict?)
             (let-values ([(acc _) (ms-pop-remain acc)])
               acc))))
  (foldl apply-single state matchers))

(define (apply-to-completion -matcher xmls)
  (define matcher
    (if (symbol? -matcher) (data-matcher -matcher) -matcher))
  (define (apply-to-completion* ele)
    (match ele
      [(? get-tag x)
       (define-values (tag contents) (decompose-element x))
       (with-tag tag (apply-to-completion matcher contents))]
      [else #f]))
  (define init-state (ms-only-remain xmls))
  (define (drain-into parent state)
    (define new-state (ms-clone-remaining state))
    (define ans (apply-matchers (list matcher) new-state))
    (cond
      [(and ans (ms-has-remaining? ans))
       (drain-into (ms-add-clean-child parent ans) ans)]
      ;; TODO possibly pop remaining by one and try another match
      ;; in the case that there was a complete failure, and the passed
      ;; in state has some remaining?
      [(not ans) #f]
      [else (ms-add-child parent ans)]))
  (define main-ans (bail-to-fail (drain-into ms-empty init-state)))
  (define actual-ans-list (cons main-ans (map apply-to-completion* xmls)))
  (define condensed-ans
    (foldl (λ (nxt state) (if nxt (ms-add-child nxt state) state))
           ms-empty
           actual-ans-list))
  (and (not (ms-empty? condensed-ans))
       condensed-ans))


(struct data-matcher (name)
  #:methods gen:matcher
  [(define (attempt-match self acc)
     (ms-pop&assign acc (data-matcher-name self)))])

(define (simple-tag-matcher name . subs)
  (simple-tag-matcher* name subs))

(struct simple-tag-matcher* (tag-name sub-matchers)
  #:methods gen:matcher
  [(define (attempt-match self orig-state)
     (match-define (simple-tag-matcher* my-name my-subs) self)
     (define-values (post-state ele) (ms-pop-remain orig-state))
     (define (tag=? x) (eq? my-name (get-tag x)))
     (define (attributes=? x) #t) ;; TODO
     (define (content-len=? x) (equal? (length (xml-content x))
                                       (length my-subs)))
     (match ele
       [(? tag=? (? attributes=? (? content-len=? the-ele)))
        (define-values (pre-state old-length)
          (ms-prepend-content post-state (xml-content the-ele)))
        (define (next-matcher matcher state)
          (and state (apply-matchers (list matcher) state #:strict #t)))
        (define final-state
          (with-tag my-name (foldl next-matcher pre-state my-subs)))
        (and final-state
             (equal? (ms-remain-length final-state)
                     old-length)
             final-state)]
       [else (bail-out #f)]))])

(define TAG-CM 'tag)


(define/contract (ms-pop&assign ms name)
  (-> match-state? symbol? (or/c match-state? false?))
  (define-values (state ele) (ms-pop-remain ms))
  (cond
    [ele
     (define path (reverse (extract-current-continuation-marks TAG-CM)))
     (define mdm (match-state-mdm state))
     (define remain (match-state-remain state))
     (match-state (mdm-join mdm (mdm-with [name (match-data path (xml->string ele))]))
                  remain)]
    [else #f]))

(define (extract-current-continuation-marks key)
  (continuation-mark-set->list
   (current-continuation-marks)
   key))

(define-syntax with-tag
  (syntax-parser
    [(_ t:id e:expr)
     #'(with-continuation-mark TAG-CM t
       e)]))
