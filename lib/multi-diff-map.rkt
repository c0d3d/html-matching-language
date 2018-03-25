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
 ; Functions
 mdm-empty
 mdm-empty?
 mdm-set
 mdm-add-child
 mdm-join
 mdm-ref
 make-mdm
 mdm-set-children
 mdm-get-children
 base-count
 build-mdm

 ; Syntax
 mdm-with)

(require racket/hash)
(require (for-syntax syntax/parse))

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

;; ImmutableHash [Listof MDiffMap] -> MDiffMap
;; Simple constructor for mdm's
(define (make-mdm h children)
  (list h children))

;; MDiffMap [Listof MDiffMap] -> MDiffMap
;; Functionally sets the children of the given mdm
(define (mdm-set-children m children)
  (match-define (list h _) m)
  (list h children))

;; MDiffMap -> Boolean
;; Is the given MDiffMap *THE* empty map?
;; Compared by reference since we should only have
;; one empty map.
(define (mdm-empty? mdm)
  (eq? mdm mdm-empty))
(module+ test
  (check-true (mdm-empty? mdm-empty))
  (check-false (mdm-empty? (mdm-with ['a 'b])))
  (check-false (mdm-empty? (mdm-add-child mdm-empty (mdm-with ['a 'b])))))

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
;; Note that empty nodes along the path won't have any affect
;; on the result of this function.
(define (build-mdm mdm)
  (match-define (list hash children) mdm)
  (cond
    [(and (hash-empty? hash) (empty? children))
     (list)]
    [(hash-empty? hash) (append-map build-mdm children)]
    [(andmap mdm-empty? children) (list hash)]
    [else
     (for/list ([child (append-map build-mdm children)])
       (hash-union hash child))]))

(module+ test
  (check-equal? (build-mdm mt-node) (list))
  (check-equal? (build-mdm (mdm-add-child mt-node mt-node)) (list))
  (check-equal? (build-mdm (mdm-add-child (mdm-add-child mt-node mt-node) ab-node))
                (list #hash((a . b))))
  (check-equal? (build-mdm ab-cd-node)
                (list #hash((a . b) (c . d))))
  (check-equal? (build-mdm ab-node) (list #hash((a . b))))
  (check-equal?
   (build-mdm mn-<kl-<ef-gh+ij>>+<ab-cd>-node)
   (list #hash((m . n) (k . l) (e . f) (g . h))
         #hash((m . n) (k . l) (e . f) (i . j))
         #hash((m . n) (a . b) (c . d)))))

;; MDiffMap -> Nat
;; Counts the number of nodes at the base of the tree
(define (base-count m)
  (match-define (list h children) m)
  (cond
    [(and (not (hash-empty? h)) (empty? children)) 1]
    [else (foldl + 0 (map base-count children))]))

;; Syntax for easily making a multi-diff-map
(define-syntax mdm-with
  (syntax-parser
    [(_ (k1:expr v1:expr) (k-rest:expr v-rest:expr) ...)
     #'(mdm-set (mdm-with (k-rest v-rest) ...) k1 v1)]
    [(_)
     #'mdm-empty]))

(module+ test
  (check-equal? (base-count mt-node) 0)
  (check-equal? (base-count ab-cd-node) 1)
  (check-equal? (base-count ef-gh+ij-node) 2)
  (check-equal? (base-count mn-<kl-<ef-gh+ij>>+<ab-cd>-node) 3))

;; MDiffMap Key -> [Or #f Value]
;; Returns either #f (if the given mdm doesn't map anything with the given key),
;; or the mapped value for the given key.
(define (mdm-ref mdm k)
  (match-define (list h children) mdm)
  (if (hash-has-key? h k)
      (hash-ref h k)
      (ormap (λ (x) (mdm-ref x k)) children)))

(module+ test
  (check-equal? (mdm-ref mn-<kl-<ef-gh+ij>>+<ab-cd>-node 'z) #f)
  (check-equal? (mdm-ref mn-<kl-<ef-gh+ij>>+<ab-cd>-node 'a) 'b)
  (check-equal? (mdm-ref mn-<kl-<ef-gh+ij>>+<ab-cd>-node 'e) 'f)
  (check-equal? (mdm-ref mn-<kl-<ef-gh+ij>>+<ab-cd>-node 'm) 'n)
  (check-equal? (mdm-ref mn-<kl-<ef-gh+ij>>+<ab-cd>-node 'h) #f)
  (check-equal? (mdm-ref mn-<kl-<ef-gh+ij>>+<ab-cd>-node 'c) 'd))

;; MDiffMap MDiffMap -> MDiffMap
;; Joins the two maps.
(define (mdm-join mdm1 mdm2)
  (match-define (list h1 c1) mdm1)
  (match-define (list h2 c2) mdm2)
  (list (hash-union h1 h2)
        (append c1 c2)))

(define mdm-get-children second)
