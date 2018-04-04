#lang racket

#;(define (ordered-sub-pat-matcher ele . subs)
  (define (the-match ele . subs)
    (define (self)
      (λ (acc cur-lvl)
        (define self-app? (should-self-apply?))
        (match
            cur-lvl
          ((nest-tag tag-name sub-xmls)
           #:when
           (and (equal? (length sub-xmls) (length subs)) (eq? ele tag-name))
           (with-continuation-mark
             TAG-CM
             tag-name
             (let ((the-ans
                    (with-continuation-mark
                      TOP-LEVEL-CM
                      #t
                      ((λ ()
                         (let/cc
                             bail
                           (define (fail) (bail acc))
                           (define (fold-one nxt-fun nxt-xml acc)
                             (cond
                               ((symbol? nxt-fun) (set-match acc nxt-fun nxt-xml))
                               (else
                                (define the-ans (nxt-fun ms-empty nxt-xml))
                                (if (or (not (ms-no-remaining? the-ans))
                                        (ms-empty? the-ans))
                                    (fail)
                                    (foldl ms-join acc (ms-get-children the-ans))))))
                           (foldl fold-one acc subs sub-xmls)))))))
               (if (and (not #f) self-app?)
                   (match/foldl
                    (self)
                    the-ans
                    (filter nest-tag? (map as-lvl sub-xmls)))
                   the-ans))))
          ((nest-tag tag-name sub-xmls)
           (with-continuation-mark
             TAG-CM
             tag-name
             (let ((the-ans
                    (with-continuation-mark TOP-LEVEL-CM #t ((λ () acc)))))
               (if (and (not #f) self-app?)
                   (match/foldl
                    (self)
                    the-ans
                    (filter nest-tag? (map as-lvl sub-xmls)))
                   the-ans))))
          ((leaf-tag tag-data) acc))))
    (self))
  (define the-fun (apply the-match (cons ele subs)))
  (λ (acc next-xmls) (the-fun acc (as-lvl next-xmls))))
