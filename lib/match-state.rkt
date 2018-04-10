#lang racket

(require "multi-diff-map.rkt"
         "html-matcher.rkt")

(provide
 build-ms
 match-state
 match-state-mdm
 match-state-remain
 match-state?
 ms-add-child
 ms-add-clean-child
 ms-assign
 ms-clone-remaining
 ms-empty
 ms-empty?
 ms-has-remaining?
 ms-only-remain
 ms-pop-remain
 ms-prepend-content
 ms-remain-length
 )

(struct match-state (acc xmls))
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

(define (ms-assign ms k v)
  (match-define (match-state mdm remain) ms)
  (match-state (mdm-join mdm (mdm-with [k v]))
               remain))

(define/contract (ms-prepend-content ms content)
  (-> match-state? (listof content/c) (values match-state? natural?))
  (match-define (match-state mdm remain) ms)
  (values (match-state mdm (append content remain))
          (length remain)))

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

(define (match-state-mdm ms)
  (match-define (match-state mdm remain) ms)
  mdm)

(define (match-state-remain ms)
  (match-define (match-state mdm remain) ms)
  remain)

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
