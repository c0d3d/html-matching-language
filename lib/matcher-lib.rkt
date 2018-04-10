#lang racket

(require (for-syntax syntax/parse
                     syntax/quote
                     ))
(require racket/generic "html-matcher.rkt" "multi-diff-map.rkt" xml
         (prefix-in base: racket))

(provide
 attempt-match
 simple-tag-matcher
 data-matcher
 ms-empty
 build-ms
 apply-to-completion)

(struct match-state (acc xmls) #:transparent)
(define ms-empty (match-state mdm-empty '()))

(define/contract (ms-empty? m)
  (-> match-state? boolean?)
  ; We are always re-using the same reference
  (eq? m ms-empty))

(define (build-ms ms)
  (define result
    (cond
      [ms
       (match-define (match-state acc _) ms)
       (build-mdm acc)]
      [else '()]))
  result)

(define/contract (ms-prepend-content ms content)
  (-> match-state? (listof content/c) (values match-state? natural?))
  (match-define (match-state mdm remain) ms)
  (values (match-state mdm (append content remain))
          (length remain)))


(define bail (make-parameter #f))
(define (bail-out x)
  ((bail) x))
(define-syntax bail-to-fail
  (syntax-parser
    [(_ x ...)
     #'(let/cc k
         (parameterize ([bail k])
           x ...))]))

(define (merge-state-results s1 s2)
  (and s1 s2 (ms-join s1 s2)))

(define/contract (ms-pop&assign ms name)
  (-> match-state? symbol? (or/c match-state? false?))
  (define-values (state ele) (ms-pop-remain ms))
  (cond
    [ele
     (define path (reverse (extract-current-continuation-marks TAG-CM)))
     (match-define (match-state mdm remain) state)
     (match-state (mdm-join mdm (mdm-with [name (match-data path (xml->string ele))]))
                  remain)]
    [else #f]))

(define (ms-only-remain xmls)
  (match-state mdm-empty xmls))

(define/contract (ms-pop-remain ms)
  (-> match-state? (values match-state? (or/c false? content/c)))
  (match ms
    [(match-state mdm (list)) (values ms #f)]
    [(match-state mdm (cons hd tl)) (values (match-state mdm tl) hd)]))

;; Appends remaining XMLs as well
;; as combining accumulators (second one as a child)
(define/contract (ms-add-child ms child)
  (-> match-state? match-state? match-state?)
  (match-define (match-state acc xmls) ms)
  (match-define (match-state acc-child xmls-child) child)
  (match-state (mdm-add-child acc acc-child)
               (append xmls xmls-child)))

(define (ms-set-remain ms remain)
  (match-define (match-state a _) ms)
  (match-state a remain))

(define (state-no-remaining? ms)
  (and (match-state? ms) (empty? (match-state-xmls ms))))

(define/contract (ms-add-clean-child ms new-child)
  (-> match-state? match-state? match-state?)
  (match-define (match-state a1 x1) ms)
  (match-define (match-state a2 _)  new-child)
  (match-state (mdm-add-child a1 a2) x1))

(define (ms-pop-remaining ms)
  (match-define (match-state acc r) ms)
  (match-state acc (rest r)))

(define (ms-clone-remaining ms)
  (match-define (match-state _ r) ms)
  (match-state mdm-empty r))

(define (ms-remain-length ms)
  (match-define (match-state _ r) ms)
  (length r))


;; Joins the given match-states
;; appends xmls, join's acc's
(define/contract (ms-join ms1 ms2)
  (-> match-state? match-state? match-state?)
  (match-define (match-state acc1 xmls1) ms1)
  (match-define (match-state acc2 xmls2) ms2)
  (match-state (mdm-join acc1 acc2) xmls1 #;(append xmls1 xmls2)))

(define/contract (ms-clean-join ms1 ms2)
  (-> match-state? state-no-remaining? match-state?)
  (match-define (match-state a1 x1) ms1)
  (match-define (match-state a2 _)  ms2)
  (match-state (mdm-join a1 a2) x1))

; Does the given match-state have any remaining xmls
(define/contract (ms-has-remaining? ms)
  (-> match-state? boolean?)
  (match-define (match-state _ xmls) ms)
  (pair? xmls))

; Adds given remaining xmls
(define/contract (ms-add-remaining ms r)
  (-> match-state? (listof content/c) match-state?)
  (match-define (match-state acc xmls) ms)
  (match-state acc (append xmls r)))

(define-generics matcher
  ; Matcher MatchState -> (Or False MatchState)
  [attempt-match . (matcher acc)])

(define (get-tag xml)
  (and (element? xml) (element-name xml)))

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

(define-syntax (simple-tag-matcher stx)
  (syntax-parse stx
    [(_ tagname . remaining)
     (define matcherify
       (syntax-parser
         [((~datum quote) s)
          #'(base:#%app data-matcher 's)]
         [a #'a]))
     #`(base:#%app simple-tag-matcher*
                   tagname
                   (list #,@(map matcherify (syntax->list #'remaining))))]))

(define-syntax with-tag
  (syntax-parser
    [(_ t:id e:expr)
     #'(with-continuation-mark TAG-CM t
       e)]))

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
(define (extract-current-continuation-marks key)
  (continuation-mark-set->list
   (current-continuation-marks)
   key))
