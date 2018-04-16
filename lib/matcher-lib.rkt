#lang racket

(require (for-syntax syntax/parse
                     syntax/quote
                     ))
(require "html-matcher.rkt"
         "multi-diff-map.rkt"
         "match-state.rkt"
         "wildcard-expander.rkt"
         "matcher-def.rkt"
         xml
         racket/generic
         (prefix-in base: racket))

(provide
 always-match
 simple-tag-matcher
 wildcard-matcher
 data-matcher
 apply-to-completion
 always-match?)


(define TAG-CM 'tag)
(define bail (make-parameter #f))

(define (bail-out x)
  ((bail) x))

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
     (pop&assign acc (data-matcher-name self)))
   (define (get-consume-range self) (cons 1 2))])

(define (simple-tag-matcher name attrs . subs)
  (simple-tag-matcher* name subs attrs))

(struct simple-tag-matcher* (tag-name sub-matchers attr-list)
  #:methods gen:matcher
  [(define (attempt-match self orig-state)
     (match-define (simple-tag-matcher* my-name my-subs my-attrs) self)
     (define-values (post-state ele) (ms-pop-remain orig-state))
     (define (tag=? x) (eq? my-name (get-tag x)))
     (define (attributes=? x)
       (define ele-attrs (list-element-attrs x))
       (define my-keys (map car my-attrs))

       (and ele-attrs
            (let ((selected (map (λ (x) (assoc x ele-attrs)) my-keys)))
              (and (andmap identity selected)
                   (andmap
                    (λ (x) (equal? (cdr (assoc (car x) my-attrs))
                                   (cdr x)))
                    selected)))))
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
       [else (bail-out #f)]))
   (define (get-consume-range this) (cons 1 2))])


(struct always-matcher* ()
  #:methods gen:matcher
  [(define (attempt-match self state)
     (ms-drop-remain state))
  (define (get-consume-range this) (cons #f #f))])

(define always-match (always-matcher*))
(define (always-match? m)
  (eq? m always-match))

(define (wildcard-matcher name attrs . subs)
  (wildcard-matcher* name subs attrs))

(struct wildcard-matcher* (name subs attrs)
  #:methods gen:matcher
  [(define/generic gen-range get-consume-range)
   (define (attempt-match self state)
     ;; TODO state
     (match-define (wildcard-matcher* name subs _) self)
     (define remaining (match-state-remain state))
     (define i-allow-zero?
       (let ([r (get-consume-range self)])
         (or (not (car r)) (equal? 0 (car r)))))
     (cond
       [(and i-allow-zero? (empty? (match-state-remain state))) state]
       [(empty? (match-state-remain state)) #f]
       [else
        (define wildcard-tree
          (handle-next (first subs) (rest subs) (match-state-remain state)))
        (define results (perform-match-tree wildcard-tree (match-state-remain state)))
        (displayln (format "Perform-Match-Results: ~a" results))
        (and results (ms-add-child state results))])) ; TODO one wildcard child next to another ...
   (define (get-consume-range self)
     (define (select-range x)
       (and (not (always-match? x))
            (car (gen-range x))))
     (define min
       (foldl + 0 (filter-map select-range (wildcard-matcher*-subs self))))
     (define max
       (if (ormap always-match? (wildcard-matcher*-subs self))
           #f
           (length (wildcard-matcher*-subs self))))
     (cons min max))])


(define/contract (pop&assign ms name)
  (-> match-state? symbol? (or/c match-state? false?))
  (define-values (state ele) (ms-pop-remain ms))
  (cond
    [ele
     (define path (reverse (extract-current-continuation-marks TAG-CM)))
     (define data (match-data path (xml->string ele)))
     (ms-assign state name data)]
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

(define-syntax bail-to-fail
  (syntax-parser
    [(_ x ...)
     #'(let/cc k
         (parameterize ([bail k])
           x ...))]))
