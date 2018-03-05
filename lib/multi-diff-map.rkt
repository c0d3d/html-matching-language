#lang racket

;; This module creates a structure that is sort of a map-tree.
;; The "map-tree" meaning that it is tree with maps at the nodes
;; and no data at the leaves. The semantics of this tree is that it
;; represents a set of maps, where each node will be unioned with
;; each of its children idividually yielding a seperate match for each.
;; This is kind of a "diff list" for maps except there are multiple maps
;; being constructed in parallel. A final "build" function will traverse
;; the tree and produce a list of maps where each map corresponds to the union
;; of all the nodes along a unique path from the root of the tree to a base node.
;; There is one map for each such path. See `html-matcher` for why
;; this could possibly be useful.

(provide
; mdm-with
 mdm-empty
 mdm-set
 mdm-add-child
 base-count
 build)

(require racket/hash)

(module+ test
  (require rackunit)
  (define mt-hash (make-immutable-hash '()))
  (define ab-hash (make-immutable-hash '((a . b))))
  (define cd-hash (make-immutable-hash '((c . d))))
  (define ef-hash (make-immutable-hash '((e . f))))
  (define gh-hash (make-immutable-hash '((g . h))))
  (define ij-hash (make-immutable-hash '((i . j))))
  (define kl-hash (make-immutable-hash '((k . l))))
  (define mn-hash (make-immutable-hash '((m . n))))

  (define mt-node (list mt-hash '()))
  (define ab-node (list ab-hash '()))
  (define cd-node (list cd-hash '()))

  (define ef-node (list ef-hash '()))
  (define gh-node (list gh-hash '()))
  (define ij-node (list ij-hash '()))
  (define kl-node (list kl-hash '()))

  (define ab-cd-node (list ab-hash `(,cd-node)))
  (define ef-gh+ij-node (list ef-hash `(,gh-node ,ij-node)))
  (define kl-<ef-gh+ij>-node (list kl-hash `(,ef-gh+ij-node)))

  ;; One more complicated example.
  (define mn-<kl-<ef-gh+ij>>+<ab-cd>-node
    (list mn-hash `(,kl-<ef-gh+ij>-node ,ab-cd-node))))

;; An MDiffMap is a [List ImmutableHashMap [Listof MDiffMap]].
;; This represents a node in our map-tree that has yet to be finalized

;; MDiffMap
;; The empty MDiffMap
(define mdm-empty
  (list (make-immutable-hash '()) '()))

;; MDiffMap MDiffMap -> MDiffMap
;; Extends the first mdm with the second as a child.
(define (mdm-add-child mdm child)
  (match-define (list hash children) mdm)
  (list hash (cons child children)))

(module+ test
  (check-equal?
   (mdm-add-child mt-node ab-node) (list mt-hash (list ab-node)))
  (check-equal?
   (mdm-add-child ab-node cd-node) ab-cd-node)
  (check-equal?
   (mdm-add-child (list ef-hash `(,ij-node)) gh-node) ef-gh+ij-node)
  (check-equal?
   (mdm-add-child mn-<kl-<ef-gh+ij>>+<ab-cd>-node ab-node)
   (list mn-hash `(,ab-node ,kl-<ef-gh+ij>-node ,ab-cd-node)))
  (check-equal?
   (mdm-add-child mt-node mt-node)
   (list mt-hash (list mt-node))))

;; MDiffMap key val -> MDiffMap
;; Extends the given MDiffMap with a new key-value mapping.
(define (mdm-set mdm k v)
  (match-define (list hash children) mdm)
  (list (hash-set hash k v) children))

(module+ test
  (check-equal? (mdm-set mt-node 'a 'b) ab-node)
  (check-equal?
   (mdm-set ab-node 'c 'd) (list #hash((a . b) (c . d)) '()))
  (check-equal?
   (mdm-set ab-cd-node 'c 'd) (list #hash((a . b) (c . d)) (list cd-node))))

;; MDiffMap -> [Listof ImmutableHashMap]
;; Collapses the map tree into individual hashes.
;; There is one hash in the returned list for every path
;; from the root of the tree to a base node. The hashes
;; are the union of all the nodes along each path.
(define (build mdm)
  (match-define (list hash children) mdm)
  (if (empty? children)
      (list hash)
      (for/list ([child (append-map build children)])
        (hash-union hash child))))

(module+ test
  (check-equal? (build mt-node) (list #hash()))
  (check-equal? (build (mdm-add-child mt-node mt-node)) (list #hash()))
  (check-equal? (build (mdm-add-child (mdm-add-child mt-node mt-node) ab-node))
                (list #hash((a . b)) #hash()))
  (check-equal? (build ab-cd-node)
                (list #hash((a . b) (c . d))))
  (check-equal? (build ab-node) (list #hash((a . b))))
  (check-equal?
   (build mn-<kl-<ef-gh+ij>>+<ab-cd>-node)
   (list #hash((m . n) (k . l) (e . f) (g . h))
         #hash((m . n) (k . l) (e . f) (i . j))
         #hash((m . n) (a . b) (c . d)))))

;; [Or MDiffMap MDiffMapSealed] -> Nat
;; Counts the number of nodes at the base of the tree
(define (base-count m)
  (match-define (list _ children) m)
  (cond
    [(empty? children) 1]
    [else (foldl + 0 (map base-count children))]))

(module+ test
  (check-equal? (base-count mt-node) 1)
  (check-equal? (base-count ab-cd-node) 1)
  (check-equal? (base-count ef-gh+ij-node) 2)
  (check-equal? (base-count mn-<kl-<ef-gh+ij>>+<ab-cd>-node) 3))

#;(define (mdm-with . pairs)
  (when (odd? (length pairs)) (error "Need pairs to make a map"))
  (define (combine-pairs l)
    (cons (cons (first l) (second l)) (pairs (rest (rest l)))))
  (define combined-pairs (combine-pairs pairs))
  (list (make-immutable-hash combined-pairs)))
