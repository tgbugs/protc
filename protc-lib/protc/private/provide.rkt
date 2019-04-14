#lang racket/base

(require (for-syntax racket/base
                     syntax/parse
                     racket/provide-transform))
(provide protc-out
         module-protc-exports
         protc-module-end)

;;; provide machinery
;; FIXME move to module definitions

(define-for-syntax protc-exports '())

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
        (set! protc-exports (append (syntax->datum #'(clauses ...)) protc-exports))
        #`(define-syntax protc-vars-info (quote-syntax #,(syntax->list #'(clauses ...))))
        ])]))

;;; macros needed to provide protc-exports as it is set by protc-out
; both module-protc-exports and protc-module-end are needed to work cooperatively
; to delay conversion of protc-exports until the very end of module execution
(define-syntax (module-protc-exports stx)
    (syntax-parse stx
      [_
       ; TODO pick the right identifiers for protc-exports automatically?
       ; or trust the user to get (provide (protc-out spec/*has-part?)) instead of
       ; (provide (protc-out has-part?))
       (datum->syntax stx (cons 'list protc-exports))]))

(define-syntax (protc-module-end stx)
  "used implicitly in kernel.rkt to provide protc-exports set using by protc-out"
  (syntax-parse stx
    [_
     (syntax-local-lift-module-end-declaration (datum->syntax stx '(define protc-module-out module-protc-exports)))
     #'(void 'protc-module-end-was-called-here)]))

(module+ test
  (require rackunit)
  protc-module-end
  (provide (protc-out spec/something-else-0))
  (provide (protc-out spec/something-else-2))
  (provide (protc-out spec/something-else-3))
  (provide (protc-out spec/something-else-1))

  (define spec/something-else-0 'hello)
  (define spec/something-else-1 'there)
  (define spec/something-else-2 'general)
  (define spec/something-else-3 'kenobi)
  (define test-out module-protc-exports)
  (check-equal? test-out '(there kenobi general hello)))
