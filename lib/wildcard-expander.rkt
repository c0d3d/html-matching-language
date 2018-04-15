#lang racket

(require "match-state.rkt"
         "matcher-def.rkt"
         racket/match)

(provide handle-next
         perform-match-tree)

(struct wcnode (mtch children) #:transparent)

(define (only1? x)
  (and (car x)
       (cdr x)
       (eq? 1 (- (cdr x)
                 (car x)))))

(define (handle-next remain-mchs eles)
  (cond
    [(empty? remain-mchs)
     (displayln "NONE LEFT!")
     #f]
    [(only1? (get-consume-range (first remain-mchs)))
     (displayln "ONLY 1!")
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
       (for/stream ([me-consume (range (sub1 high+) (- low+ 2) -1)])
         (cons me-consume (handle-next (rest remain-mchs) (drop eles me-consume)))))
     (displayln (format "Finish-Children: ~a -> ~a \n~a\n"
                        (first remain-mchs)
                        (stream->list finished-children)
                        eles))
     (wcnode (first remain-mchs) finished-children)]))


(define (perform-match-tree wcn eles)
  (match-define (wcnode matcher cstream) wcn)
  (define (do-match child)
    (define match-result
      (match child
        [(cons my-count nxt)
         (define-values (cur-eles nxt-eles) (split-at eles my-count))
         (define a (gensym))
         (displayln (format "Attempt: ~a & ~a (~a)" matcher cur-eles a))
         (define the-cur-match (attempt-match matcher (ms-only-remain cur-eles)))
         (displayln (format "DONE: ~a" a))
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
   (stream-map
    do-match
    (stream-filter (compose identity cdr) cstream))))
