#lang racket

;; This module serves two purposes:
;;   1. Create a structure that is sort of a map-tree.
;;      The "map-tree" meaning that it is tree with maps at the nodes
;;      and no data at the leaves. The semantics of this tree is that it
;;      represents a set of maps, where each node will be unioned with
;;      each of its children idividually yielding a seperate match for each.
;;      This is kind of a "diff list" for maps except there are maps multiple
;;      being constructed in parallel. A final "build" function will traverse
;;      the tree and produce a list of maps that correspond to the union of
;;      each path to a base node from the root. See `html-matcher` for why
;;      this could possibly be useful.
;;   2. Allows us to optimize the storage and runtime of this structure.
;;      For example: A node with only one child can always be collapsed
;;      into a single node (see the "seal" function). These optimizations
;;      would be possible with a simpler list and hash based structure at
;;      the use site, but it is cleaner to build an abstraction for these
;;      things.

;; An MDiffMap is a [List ImmutableHashMap [Listof MDiffMapSealed]].
;; This represents a node in our map-tree that has yet to be finalized

;; An MDiffMapSealed (sealed-map ImmutableHashMap [Listof MDiffMapSealed])
;; This represents a node that has been "sealed", as in has been
;; finalized and optimized.

(provide

 ;; MDiffMap
 mdiff-map-empty

 ;; MDiffMap key val -> MDiffMap
 mdiff-map-set

 ;; MDiffMap MDiffMapSealed -> MDiffMap
 mdiff-map-add-child

 ;; MDiffMap -> MDiffMapSealed
 seal

 ;; MDiffMapSealed -> [Listof ImmutableHashMap]
 build)

(require racket/struct)

(module+ test
  (require rackunit (for-syntax syntax/parse))
  (define mt-hash (make-immutable-hash '()))
  (define ab-hash (im-map ['a 'b]))
  (define cd-hash (im-map ['c 'd]))
  (define wxyz-hash (im-map ['w 'x] ['y 'z]))

  (define mt-node (list mt-hash '()))
  (define ab-node (list ab-hash '()))
  (define cd-node (list cd-hash '()))
  (define wxyz-node (list wxyz-hash '()))

  (define mt-sealed (sealed-map mt-hash '()))
  (define ab-sealed (sealed-map ab-hash '()))
  (define cd-sealed (sealed-map cd-hash '()))
  (define wxyz-sealed (sealed-map wxyz-hash '()))

  (define ab-cd-node (list ab-hash `(,cd-sealed)))
  (define wxyz-ab+cd-node (list wxyz-hash `(,ab-sealed ,cd-sealed)))

  (define ab-cd-sealed (sealed-map ab-hash `(,cd-sealed)))
  (define wxyz-ab+cd-sealed (sealed-map wxyz-hash `(,ab-sealed ,cd-sealed))))

(struct sealed-map (data children) #:transparent)

;; MDiffMap
;; The empty MDiffMap
(define mdiff-map-empty
  (list (make-immutable-hash '()) '()))

(define build (void))
(define seal (void))
(define mdiff-map-add-child (void))
(define mdiff-map-set (void))

;; [Or MDiffMap MDiffMapSealed] -> [List ImmutableHashMap [Listof MDiffMapSealed]]
;; Normalizes the given input, and extracts the important information.
(define (extract m)
  (cond
    [(sealed-map? m) (struct->list m)]
    [else m]))

(module+ test
  (check-equal? (extract (list mt-hash '()))
                (list mt-hash '()))
  (check-equal? (extract (extract mt-sealed))
                (list mt-hash '()))
  (check-equal? (extract ab-node)
                ab-node)
  (check-equal? (extract ab-sealed)
                ab-node)
  (check-equal? (extract wxyz-ab+cd-sealed)
                wxyz-ab+cd-node))


;; [Or MDiffMap MDiffMapSealed] -> Nat
;; Counts the number of nodes at the base of the tree
(define (base-count m)
  (match-define (list data children) (extract m))
  (cond
    [(empty? children) 1]
    [else (foldl + 0 (map base-count children))]))

(module+ test
  (check-equal? (base-count mt-node) 1)
  (check-equal? (base-count mt-sealed) 1)

  (check-equal? (base-count ab-node) 1)
  (check-equal? (base-count ab-sealed) 1)

  (check-equal? (base-count wxyz-node) 1)
  (check-equal? (base-count wxyz-sealed) 1)

  (check-equal? (base-count ab-cd-node) 1)
  (check-equal? (base-count ab-cd-sealed) 1)

  (check-equal? (base-count wxyz-ab+cd-node) 2)
  (check-equal? (base-count wxyz-ab+cd-sealed) 2))


(module+ test
  (define-syntax im-map
    (syntax-parser
      [(_ (v1:expr v2:expr) (v3:expr v4:expr) ...)
       #'(hash-set (im-map (v3 v4) ...) v1 v2)]
      [(_)
       #'mt-hash])))
