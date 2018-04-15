#lang racket

(require (for-syntax syntax/parse racket/syntax racket syntax/stx))
(require "html-matcher.rkt"
         "matcher-lib.rkt"
         "match-state.rkt"
         (prefix-in base: racket)
         xml)
(provide
 (for-syntax make-pattern*)
 make-pattern
 match/html
 string->xml/element
 (except-out (all-from-out xml "html-matcher.rkt") attribute))

(define-for-syntax (select-matcher-for data)
  (cond
    [(ormap (λ (x . y) (eq? '* x)) data) #'wildcard-matcher]
    [else #'simple-tag-matcher]))

(define-for-syntax (parse-pattern stx)
  (syntax-parse stx
    [(~datum *) #'always-match]
    [((~datum quote) x) #'(data-matcher 'x)]
    [(tag-name:id ((key:id val:expr) ...) contents:expr ...)
     #:with matcher (select-matcher-for (syntax->datum stx))
     #`(matcher 'tag-name
                ((key val) ...)
                #,@(map parse-pattern (syntax->list #'(contents ...))))]
    [(tag-name:id contents:expr ...)
     #:with matcher (select-matcher-for (syntax->datum stx))
     #`(matcher 'tag-name
                #,@(map parse-pattern (syntax->list #'(contents ...))))]
    [s (error (syntax->datum #'s))]))

;; Compiles a pattern
(define-for-syntax (make-pattern* stx)
  (parse-pattern stx))

;; Syntax version of the compile time function
(define-syntax (make-pattern stx)
  (syntax-parse stx
    [(_ a)
     (make-pattern* #'a)]))


(define-for-syntax (check-name allowed stx)
  (unless (and (identifier? stx)
               (member (syntax-e stx) allowed))
    (define msg
      (string-append
       "Unbound identifier inside match/html. "
       "If you are trying to access pattern variables you must use a literal pattern, and "
       "have a pattern variable with the correct name."))
    (raise-syntax-error #f msg stx)))

(define-for-syntax (collect-pattern-vars stx)
  (syntax-parse stx
    [((~datum quote) e:id) (list (syntax-e #'e))]
    [(i:id ((key:id val:expr) ...) a ...)
     ; For now we cut out the extra
     (collect-pattern-vars #'(i a ...))]
    [(_:id a ...)
     (define (folder nxt acc)
       (append (collect-pattern-vars nxt) acc))
     (foldl folder '() (syntax->list #'(a ...)))]
    [x:number (list)]
    [x:id (list (syntax-e #'x))]
    [_ (list)]))

(define-for-syntax (stx-tfoldl f acc sl)
  (syntax-parse sl
    [((~datum quote) _) acc]
    [(a ...)
     (define folder (curry stx-tfoldl))
     (foldl (λ (nxt acc) (stx-tfoldl f acc nxt))
            acc (syntax->list #'(a ...)))]
    [x (f #'x acc)]))

; (identifier-binding id-stx)
(define-syntax (match/html stx)
  (syntax-parse stx
    [(_ pat:id doc:expr body:expr ...)
     #:with mm-name (format-id stx "mm")
     (define allowed-names (list (syntax-e #'mm-name)))
     (stx-tfoldl (λ (x _) (check-name allowed-names x))
                 (void)
                 #'(body ...))
     #'(for/list ([mm-name (build-ms (apply-to-completion pat (list doc)))])
         body ...)]
    [(_ pat:expr doc:expr body:expr ...)
     #:with mm-name (format-id stx "mm")
     #:attr pvars (collect-pattern-vars #'pat)
     #:attr (allowed-names-id 1) (map (λ (x) (format-id stx "~a" x))
                                       (attribute pvars))
     #:attr (allowed-names-path-id 1) (map (λ (x) (format-id stx "~a.path" x))
                                           (attribute pvars))
     #'(let* ([complete (apply-to-completion (make-pattern pat) (list doc))]
              [built (build-ms complete)])
         (for/list ([mm-name built])
           (let ([allowed-names-id
                  (match-data-text
                   (hash-ref mm-name 'allowed-names-id))] ...
                 [allowed-names-path-id
                  (match-data-path
                   (hash-ref mm-name 'allowed-names-id))] ...)
                 body ...)))]))
