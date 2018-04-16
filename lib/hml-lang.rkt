#lang racket

(require (for-syntax
          syntax/parse
          racket/syntax
          racket
          mischief/parse
          syntax/stx))

(require "html-matcher.rkt"
         "matcher-lib.rkt"
         "match-state.rkt"
         (prefix-in base: racket))
(provide
 (for-syntax make-pattern*)
 make-pattern
 match/html
 string->xml/element
 (all-from-out "html-matcher.rkt"))

;; Decides which matcher to use based on the
;; pased in s-exp. The value passed in is a syntax->datum of
;; a pattern (either sub, or top level).
(define-for-syntax (select-matcher-for data)
  (cond
    [(ormap (λ (x . y) (eq? '* x)) data) #'wildcard-matcher]
    [else #'simple-tag-matcher]))

(begin-for-syntax
  (define/contract (as-string s)
    (-> (or/c symbol? string?) string?)
    (if (not (string? s)) (symbol->string s) s)))

;; Parses out a pattern into the code that will construct
;; proper patterns (via their struct constructors)
(define-for-syntax (parse-pattern stx)
  (syntax-parse stx
    [(~datum *) #'always-match]
    [((~datum quote) x) #'(data-matcher 'x)]
    [(tag-name:id ((key:id val:expr) ...) contents:expr ...)
     #:with matcher (select-matcher-for (syntax->datum stx))
     #:attr (key+ 1) (stx-map (λ (x) (as-string (syntax->datum x))) #'(key ...))
     #:attr (val+ 1) (stx-map (λ (x) (as-string (syntax->datum x))) #'(val ...))
     #`(matcher 'tag-name
                `((,(symbol->string 'key) . ,(symbol->string 'val)) ...)
                #,@(map parse-pattern (syntax->list #'(contents ...))))]
    [(tag-name:id contents:expr ...)
     #:with matcher (select-matcher-for (syntax->datum stx))
     #`(matcher 'tag-name
                '()
                #,@(map parse-pattern (syntax->list #'(contents ...))))]))

;; Compiles a pattern (compile time function version).
(define-for-syntax (make-pattern* stx)
  (parse-pattern stx))

;; Syntax version of the pattern parser.
(define-syntax (make-pattern stx)
  (syntax-parse stx
    [(_ a)
     (make-pattern* #'a)]))

(define-for-syntax ERR-MSG
  (string-append
   "Unbound identifier. "
   "If you are trying to access pattern variables you must use a literal pattern."))

(define-for-syntax (check-name allowed stx)
  (unless (or (syntax-matches? stx _:bound-id)
              (member (syntax-e stx) allowed))
    (raise-syntax-error #f ERR-MSG stx)))

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

;; Our matching form
;; Consumes either an inlined pattern, or an identifier which references
;; a pattern, the document itself, and body expressions which will be run for
;; each match.
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
                                       (@ pvars))
     #:attr (allowed-names-path-id 1) (map (λ (x) (format-id stx "~a.path" x))
                                           (@ pvars))
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
