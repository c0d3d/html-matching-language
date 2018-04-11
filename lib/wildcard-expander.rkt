#lang racket

(require "matcher-lib.rkt" racket/match)


(struct wcnode (mtch children))

(define (only1? x)
  (and (car x)
       (cdr x)
       (eq? 1 (- (cdr x)
                 (car x)))))

(define (handle-next remain-mchs remain-eles)
  (cond
    [(and (empty? remain-mchs)
          (empty? remain-eles)) #t]
    [(empty? remain-mchs) #f] ; TODO maybe use continuation to back out
    [(only1? (first remain-mchs))
     (error)]
    [else
     (match-define (cons low high) (get-consume-range (first remain-mchs)))
     (define low+ (max 0 (or low 0)))
     (define high+
       (let ([max-len (add1 (length remain-eles))])
         (min max-len (or high max-len))))
     (define finished-children
       (for/fold ([children '()])
                 ([me-consume (range (sub1 high+) low+ -1)])
         (define-values (_ after) (split-at remain-eles me-consume))
         (define nxt (handle-next (rest remain-mchs) after))
         (if (boolean? nxt)
             children
             (cons (cons me-consume nxt) children))))
     (and (not (empty? finished-children))
          (wcnode (first remain-mchs) finished-children))]))
