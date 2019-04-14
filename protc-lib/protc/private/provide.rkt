#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     racket/provide-transform))
(provide protc-out

         )

;;; provide machinery
;; FIXME move to module definitions

(define-syntax protc-out
  ; modified from the definition of contract-out
  (make-provide-pre-transformer
   (λ (stx modes)
     (unless (or (null? modes)
                 (and (= 1 (length modes))
                      (zero? (car modes))))
       (raise-syntax-error #f
                           "allowed only in relative phase-level 0"
                           stx))
     (with-syntax ([(protc-vars-info) (generate-temporaries '(protc-vars-info))])
       (syntax-local-lift-module-end-declaration
        #`(handle-protc-out protc-vars-info #,stx))
       #'(provide-protc-vars protc-vars-info)))))

(define-syntax provide-protc-vars 
  (make-provide-transformer
   (λ (stx modes)
     (define protc-vars-info
       (syntax-parse stx
        [(_ id)
         #'id]))
     (for*/list ([provide-clause (in-list (syntax->list (syntax-local-value protc-vars-info)))]
                 [export (in-list (expand-export provide-clause modes))])
       export))))

(define-syntax (handle-protc-out stx)
  ; see collects/racket/contract/private/out.rkt for reference
  (syntax-parse stx
    [(_ protc-vars-info original-stx)
     (syntax-parse #'original-stx
       [(_ clauses ...)
        ; have to use syntax->list here to preserve sourceloc otherwise hygene prevents the mapping
        #`(define-syntax protc-vars-info (quote-syntax #,(syntax->list #'(clauses ...))))
        ])]))

(module+ test
  (define spec/something-else-0 'hello)
  (define spec/something-else-1 'there)
  (define spec/something-else-2 'general)
  (define spec/something-else-3 'kenobi)
  (provide (protc-out spec/something-else-0))
  (provide (protc-out spec/something-else-2))
  (provide (protc-out spec/something-else-3))
  (provide (protc-out spec/something-else-1)))
