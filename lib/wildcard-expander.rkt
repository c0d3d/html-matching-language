#lang racket

(require "match-state.rkt"
         "matcher-def.rkt"
         racket/match)

(provide handle-next
         perform-match-tree)

(struct wcnode (mtch children) #:transparent)

(define (only1? x)
  (define ans
    (and (car x)
         (cdr x)
         (eq? 1 (- (cdr x)
                   (car x)))))
  ans)

(define (handle-next remain-mchs eles)
  (displayln (format "Remaining: ~a" (length remain-mchs)))
  (define ans
    (cond
      [(empty? remain-mchs)
       (displayln "None left !")
       #f]
      [(only1? (get-consume-range (first remain-mchs)))
       (define nxt (handle-next (rest remain-mchs) (rest eles)))
       (wcnode (first remain-mchs)
               (if nxt (stream (cons 1 nxt)) empty-stream))]
      [else
       (match-define (cons low high) (get-consume-range (first remain-mchs)))
       (define low+ (max 0 (or low 0)))
       (define high+
         (let ([max-len (add1 (length eles))])
           (min max-len (or high max-len))))
       (define finished-children
         (for/fold ([children empty-stream])
                   ([me-consume (range (sub1 high+) (- low+ 2) -1)])

           (stream-cons
            (begin
              (displayln (format "Me-Consume: ~a" me-consume))
              (cons me-consume
                    (handle-next (rest remain-mchs) (drop eles me-consume))))
            children)))
       (define strm finished-children)
       (displayln (format "Finished: ~a" (stream->list strm)))
       (wcnode (first remain-mchs) strm)]))
  (displayln (format "The-Ans: ~a" ans))
  ans)

(define (perform-match-tree wcn eles)
  (match-define (wcnode matcher cstream) wcn)
  (define (do-match child)
    (define match-result
      (match child
        [(cons my-count nxt)
         (define-values (cur-eles nxt-eles) (split-at eles my-count))
         (displayln (format "Attempt: ~a & ~a" matcher cur-eles))
         ;; TODO only remain seems to be broken

         (define the-cur-match (attempt-match matcher (ms-only-remain cur-eles)))
         (cond
           [(and the-cur-match (not (empty? cur-eles)))
            (define perfed (perform-match-tree nxt nxt-eles))
            (if perfed ;; TODO maybe a problem
                (ms-add-child the-cur-match perfed)
                the-cur-match)]
           [the-cur-match the-cur-match]
           [else #f])]
        [else #f]))
    (displayln (format "Do-Match-Result: ~a" match-result))
    match-result)
  (define (fold-states nxt acc)
    (displayln (format "Fold-States: ~a" nxt))
    (cond
      [(and (not acc) nxt) (ms-add-child ms-empty nxt)]
      [(not nxt) acc]
      [else
       (ms-add-child acc nxt)]))
  (displayln (format "Stream: ~a" (stream->list cstream)))
  (stream-fold
   fold-states
   #f
   (stream-filter
    (compose not identity)
    (stream-map do-match cstream))))
