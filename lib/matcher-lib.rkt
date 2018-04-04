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

(define (build-ms ms)
  (cond
    [ms
     (match-define (match-state acc _) ms)
     (build-mdm acc)]
    [else '()]))

(define (merge-state-results s1 s2)
  (and s1 s2 (ms-join s1 s2)))

;; Appends remaining XMLs as well
;; as combining accumulators (second one as a child)
(define/contract (ms-add-child ms child)
  (-> match-state? match-state? match-state?)
  (match-define (match-state acc xmls) ms)
  (match-define (match-state acc-child xmls-child) child)
  (match-state (mdm-add-child acc acc-child)
               (append xmls xmls-child)))

(define (state-no-remaining? ms)
  (and (match-state? ms) (empty? (match-state-xmls ms))))

(define/contract (ms-add-clean-child ms new-child)
  (-> match-state? state-no-remaining? match-state?)
  (match-define (match-state a1 x1) ms)
  (match-define (match-state a2 _)  new-child)
  (match-state (mdm-add-child a1 a2) x1))

(define (ms-pop-remaining ms)
  (match-define (match-state acc r) ms)
  (match-state acc (rest r)))


;; Joins the given match-states
;; appends xmls, join's acc's
(define/contract (ms-join ms1 ms2)
  (-> match-state? match-state? match-state?)
  (match-define (match-state acc1 xmls1) ms1)
  (match-define (match-state acc2 xmls2) ms2)
  (match-state (mdm-join acc1 acc2) (append xmls1 xmls2)))

(define/contract (ms-clean-join ms1 ms2)
  (-> match-state? state-no-remaining? match-state?)
  (match-define (match-state a1 x1) ms1)
  (match-define (match-state a2 _)  ms2)
  (match-state (mdm-join a1 a2) x1))

; Does the given match-state have any remaining xmls
(define/contract (ms-has-remaining? ms)
  (-> match-state? boolean?)
  (match-define (match-state _ xmls) ms)
  (empty? xmls))

; Adds given remaining xmls
(define/contract (ms-add-remaining ms r)
  (-> match-state? (listof content/c) match-state?)
  (match-define (match-state acc xmls) ms)
  (match-state acc (append xmls r)))

(define-generics matcher
  ; Matcher MatchState [Listof Xml] -> (Or False MatchState)
  [attempt-match . (matcher acc elements)])

(define (get-tag xml)
  (and (element? xml) (element-name xml)))

;; (define (maybe-self f xml-tag xmls acc)
;;   (define self-apply? (empty? (extract-current-continuation-marks TAG-TL)))
;;   (if (and self-apply? acc)
;;       (broadcast f xml-tag xmls acc)
;;       acc))

(define/contract (apply-matchers* matchers xmls acc)
  (-> (listof matcher?) (listof content/c) match-state? (or/c match-state? false?))
  (define/contract (apply-single* matcher cur-state)
    (-> matcher? match-state? (or/c false? match-state?))
    (attempt-match matcher cur-state (match-state-xmls cur-state)))
  (define (apply-single matcher acc)
    (and acc (apply-single* matcher acc)))
  ; Run once through all the matchers
  ; This yields #f if any of the matchers flat-out failed
  ; otherwise yields the accumulated matches as well as any remaining
  ; xmls that need to be consumed
  (merge-state-results acc (foldl apply-single (match-state mdm-empty xmls) matchers)))

;; Applies the given matchers, and will fail if there is remaining xml to be consumed
(define (apply-to-completion matcher xmls)
  #;(define (apply-to-completion* matcher xmls acc)
    (define (result-of xml)
      (define tg (get-tag xml))
      (define nxt
        (if tg
            (with-tag tg
              (apply-to-completion matcher (xml-content xml))) ; This might need to be fresh
            ms-empty))
      (if nxt
          (or (and (state-no-remaining? nxt) nxt) ms-empty)
          ms-empty))
    (define app-result (apply-matchers* (list matcher) xmls ms-empty))
    (displayln (format "App result ~a" app-result))
    (define others (foldl (λ (nxt acc) (if nxt (ms-add-child acc nxt) acc))
                          ms-empty
                          (map result-of xmls)))
      (ms-add-child (ms-add-child acc (or app-result ms-empty)) (or others ms-empty)))
  (define (run x)
    (define t (get-tag x))
    (and
     t
     (with-tag t (apply-to-completion matcher (xml-content x)))))
  (define main-ans (apply-matchers* (list matcher) xmls ms-empty))
  (define sub-anses (map run xmls))
  (foldl ms-join ms-empty
         (filter (λ (x) x) (cons main-ans sub-anses))))

  #;(apply-to-completion* matcher xmls ms-empty)


(struct data-matcher (name)
  #:methods gen:matcher
  [(define (attempt-match self acc elements)
     (displayln (format "Found data: ~a ~a" acc elements))
     (set-match acc (data-matcher-name self) elements))])

(define-syntax (simple-tag-matcher stx)
  (syntax-parse stx
    [(_ tagname . remaining)
     (define matcherify
       (syntax-parser
         [((~datum quote) s)
          #'(base:#%app data-matcher 's)]
         [a #'a]))
     #`(base:#%app simple-tag-matcher* tagname (list #,@(map matcherify (syntax->list #'remaining))))]))

(define-syntax with-tag
  (syntax-parser
    [(_ t:id e:expr)
     #'(with-continuation-mark TAG-CM t
       e)]))

(struct simple-tag-matcher* (tag-name sub-matchers)
  #:methods gen:matcher
  [(define (attempt-match self acc elements)
     (define (attempt-match-on-tag cur-tag-name content remaining)
       (match-define (simple-tag-matcher* my-tag-name sub-ms) self)
       (when (eq? cur-tag-name my-tag-name)
         (displayln (format "~a == ~a" sub-ms remaining)))
       (if (and (eq? cur-tag-name my-tag-name)
                (equal? (length sub-ms) (length content)))
           (begin
             (displayln (format "Maybe a match?"))
             (let ([match-result
                  (with-tag my-tag-name
                    (apply-matchers* sub-ms content ms-empty #;(ms-add-remaining acc remaining)))])
             (if (and match-result (not (ms-has-remaining? match-result)))
                 (ms-add-child acc match-result)
                 #f)))
           #f))
     (match elements
       [(cons hd tl)
        (define t (get-tag hd))
        (define a (if t (attempt-match-on-tag t (xml-content hd) tl) #f))
        (displayln (format "SMP: ~a" a))
        a]
       [else #f]))])



;; (define/contract (apply-matcher matcher xml acc)
;;   (-> matcher? content/c match-state? (or/c match-state? false?))
;;   (define xml-tag (get-tag xml))
;;   (define the-ans
;;     (with-continuation-mark TAG-TL #t
;;       (if xml-tag
;;           (attempt-tag-match matcher acc xml-tag (xml-content xml))
;;           (attempt-data-match matcher acc xml))))
;;   (if xml-tag
;;       (maybe-self matcher xml-tag (xml-content xml) (or the-ans acc))
;;       the-ans))

;; (define/contract (broadcast matcher xml-tag xmls acc)
;;   (-> matcher? (or/c symbol? false?) (listof content/c) match-state? (or/c match-state? false?))
;;   (define caller
;;     (if xml-tag
;;         (λ (x) (with-continuation-mark TAG-CM xml-tag (x)))
;;         (λ (x) (x))))
;;   (define (fold-one nxt acc)
;;     (define ans (caller (thunk (apply-matcher matcher nxt acc))))
;;     (if ans ans acc))
;;   (foldl fold-one acc xmls))

;; (define (join-all l)
;;   (foldl mdm-join mdm-empty l))

;; (define/contract (all-success matchers elements acc)
;;   (-> (listof matcher?) (listof content/c) match-state? (or/c match-state? false?))
;;   (define/contract (fold-one matcher element acc)
;;     (-> matcher? content/c (or/c false? list?) (or/c list? false?))
;;     (define ans (apply-matcher matcher element mdm-empty))
;;     (and acc ans (mdm-join acc ans)))
;;   (and (equal? (length matchers) (length elements))
;;        (let ([child (foldl fold-one mdm-empty matchers elements)])
;;          (and child
;;               (mdm-add-child acc (join-all (mdm-get-children child)))))))

;; (struct exact-nested-matcher (tag-name inside-matchers)
;;   #:methods gen:matcher
;;   [(define (attempt-data-match self acc data) #f)
;;    (define (attempt-tag-match self acc tname eles)
;;      (match-define (exact-nested-matcher my-tag my-matchers) self)
;;      (and (eq? my-tag tname)
;;           (with-continuation-mark TAG-CM my-tag
;;             (all-success my-matchers eles acc))))])

;; (struct exact-nested-matcher (tag-name inside-matchers)
;;   #:methods gen:matcher
;;   [(define (attempt-data-match self acc data) #f)
;;    (define (attempt-tag-match self acc tname eles)
;;      (match-define (exact-nested-matcher my-tag my-matchers) self)
;;      (and (eq? my-tag tname)
;;           (with-continuation-mark TAG-CM my-tag
;;             (all-success my-matchers eles acc))))])

;; (struct only-data-matcher (name)
;;   #:methods gen:matcher
;;   [(define (attempt-tag-match self acc tname tcontent) #f)
;;    (define (attempt-data-match self acc data)
;;      (set-match acc (only-data-matcher-name self) data))])

(define TAG-CM 'tag)
(define TAG-TL 'top-level)

(define (set-match ms name dat #:as-child [as-child? #t])
  (define path (reverse (extract-current-continuation-marks TAG-CM)))
  (define dat-as-string
    (if (list? dat) (xml/list->string dat) (xml->string dat)))
  (define new-state
    (match-state (mdm-with [name (match-data path dat-as-string)]) '()))
  (if as-child?
      (ms-add-child ms new-state)
      (ms-join ms new-state)))

(define (extract-current-continuation-marks key)
  (continuation-mark-set->list
   (current-continuation-marks)
   key))
