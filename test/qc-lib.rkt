#lang racket

(require (for-syntax syntax/parse)
         quickcheck)

(provide
 ; Functions
 choose-one-gen
 choose-short-list
 choose-ascii-string
 cat-gen

 ; Syntax
 match-λ1
 bind-chain
 define-chained-gen)

(define-syntax match-λ1
  (syntax-parser
    [(_ pat body ...)
     #'(λ (arg) (match arg (pat body ...)))]))

;; Simple syntax for binding a set of generators
;; Inspired by Haskell's do :P
(define-syntax bind-chain
  (syntax-parser
    [(_ (name1:expr (~datum <-) gen1:expr)
        (name:expr (~datum <-) gen:expr) ...
        answer:expr)
     #'(generator-bind
        gen1
        (match-λ1 name1 (bind-chain (name <- gen) ... answer)))]
    [(_ answer:expr)
     #'(generator-unit answer)]))

(define-syntax define-chained-gen
  (syntax-parser
    [(_ dname:id (name:expr (~datum <-) gen:expr) ... answer)
     #'(define dname
         (letrec
             ([dname
               (bind-chain
                (name <- gen) ...
                answer)])
           dname))]))

;; [Listof [Gen x]] -> [Gen x]
(define (choose-one-gen options)
  (generator-bind
   (choose-integer 0 (sub1 (length options)))
   (λ (idx) (list-ref options idx))))

(define (choose-short-list g)
  (bind-chain
   [len <- (choose-integer 0 2)]
   [l <- (choose-list g len)]
   l))

;; Integer -> [Gen String]
(define (choose-ascii-string len)
  (choose-string choose-printable-ascii-char len))

;; [Listof [Gen x]] -> [Gen [Listof x]]
(define (cat-gen listg)
  (define (foldone nxt acc)
    (generator-bind
     nxt
     (λ (x) (generator-bind
             acc
             (λ (y) (generator-unit (cons x y)))))))
  (foldr foldone (generator-unit '()) listg))

(define gensym-gen
  (generator
   (λ (num r)
     (gensym))))
