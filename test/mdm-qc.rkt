#lang racket

(module+ test (require rackunit rackunit/quickcheck))
(require "../lib/multi-diff-map.rkt"
         "qc-lib.rkt"
         quickcheck
         quickcheck/private/random)

(define-chained-gen hash-gen
  [kv-count <- (choose-integer 0 2)]
  [keys <- (choose-list (choose-ascii-string 20) kv-count)]
  [vals <- (choose-list (choose-ascii-string 2) kv-count)]
  (make-immutable-hash (map cons keys vals)))

(define-chained-gen mdm-gen-no-child
  [h <- hash-gen]
  (make-mdm h '()))

(define (fixup-mdms mdm-list)
  (define (fix-one mdm)
    (bind-chain
     [cs <- (choose-short-list mdm-gen)]
     (mdm-set-children mdm cs)))
  (map fix-one mdm-list))

(define-chained-gen mdm-gen
  [children <- (choose-short-list mdm-gen-no-child)]
  [h <- hash-gen]
  [fixed-children <- (cat-gen (fixup-mdms children))]
  (make-mdm h fixed-children))

(define prop_example
  (property
   ([m mdm-gen])
   (equal? m m)))

(define prop_base-count=build-list-len
  (property
   ([mdm mdm-gen])
   (equal? (length (build-mdm mdm))
           (base-count mdm))))

(module+ test
  (check-property prop_example)
  (check-property prop_base-count=build-list-len))
