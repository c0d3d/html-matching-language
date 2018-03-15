#lang racket

(require (for-syntax syntax/parse racket))
(require "../lib/html-matcher.rkt"
         "../lib/multi-diff-map.rkt"
         rackunit
         xml
         #;(except-in syntax/parse attribute))

(provide
 (except-out
  (all-from-out racket) #%module-begin)
 (all-from-out
  xml
  rackunit
  "../lib/html-matcher.rkt"
  "../lib/multi-diff-map.rkt")
 (rename-out [tl-mod-begin #%module-begin])
 test)

;; (A -> Bool) (A -> B) -> (A -> [Or A B])
;; Runs the function only if the predicate holds
(define-for-syntax (skip? p? f)
  (λ (x) (if (p? x) (f x) x)))

(define-for-syntax (process-mod contents)
  (define (good? x)
    (let ([t (syntax->datum x)])
      (and (symbol? (first t)) (eq? 'test (first t)))))
  (define (proc-one x)
    (define x* (syntax->list x))
    (define expanded (local-expand #`(mk-matcher #,(second x*)) 'expression #f))
    #`(#,(first x*) #,expanded #,@(cddr x*)))
  (define mapper (skip? good? proc-one))
  (map mapper contents))

(define-syntax tl-mod-begin
  (syntax-parser
    [(_ all ...)
     #`(#%module-begin #,@(process-mod (syntax->list #'(all ...))))]))

(define-syntax (mk-matcher stx)
  (syntax-parse stx
    [(_ ((~datum quote) a ...)) #'(quote a ...)]
    [(_ name:id) #'name]
    [(_ (tag:id inside:expr ...))
     #`(sub-pat-matcher
        'tag
        #,@(map (λ (x) #`(mk-matcher #,x)) (syntax->list #'(inside ...))))]))

(define-syntax test
  (syntax-parser
    [(_ m:expr xexpr ((~seq k:expr (path:expr ...) v:expr) ...) ...)
     #'(check-equal?
        (build (m mdm-empty (xexpr->xml xexpr)))
        (list (make-immutable-hash (list (cons k (match-data (list 'path ...) v)) ...)) ...))]))
