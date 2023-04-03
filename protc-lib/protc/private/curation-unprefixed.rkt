#lang racket/base

(require
 ;"direct-model.rkt"
 ;rdf/utils
 (only-in "identifier-functions.rkt" hyp:)
 (for-syntax
  racket/base
  racket/path
  racket/syntax
  syntax/parse
  syntax/warn
  "utils.rkt"
  ;"identifier-functions.rkt"
  ;(except-in "direct-model.rkt" #%top)
  "syntax-classes.rkt"))

(provide
 hyp: ; FIXME needed for how we handle prov, not strictly required
 (all-defined-out))

;;; ontology tags

(define-syntax (define-tag stx-0)
  (syntax-parse stx-0
    [(_ tag-name:id)
     #'(define-syntax (tag-name stx)
         (syntax-parse stx
           [(_ content prov:sc-cur-hyp)
            #'(void)
            ])
         )]))

(define-syntax (define-tags stx)
  (syntax-parse stx
    [(_ tag:id ...)
     #'(begin (define-tag tag) ...)
     ]))

(define-tags ilxtr:technique)

;;; utility tags

(define-syntax (TODO stx)
  (syntax-parse stx
    [section:sc-cur-todo
     #:with (errors ...)
     (make-errors [#f
                   stx
                   (format "WARNING: TODO ~a at ~a ~a ~a"
                           (syntax->datum #'section.text)
                           ; FIXME section.term.label ...
                           (let ([src (syntax-source #'section)])
                             (if (not (or (path-string? src) (path-for-some-system? src)))
                                 'stdin
                                 (file-name-from-path src)))
                           (syntax-line #'section)
                           (syntax-column #'section))
                   #:kind protc-warning  ; TODO
                   #:fix #f])
     #'(begin errors ...)]))

(module+ test
  (TODO "I have no idea what this means." (hyp: 'very-todo)))

(define-syntax (circular-link stx)
  (syntax-parse stx
    [section:sc-cur-circular-link
     ;#:do ((println #'section.warning))
     #:with (errors ...)
     (make-errors [#f
                   stx
                   (format "~a at ~a ~a ~a"
                           (syntax->datum #'section.warning)
                           ; FIXME section.term.label ...
                           (let ([src (syntax-source #'section)])
                             (if (not (or (path-string? src) (path-for-some-system? src)))
                                 'stdin
                                 (file-name-from-path src)))
                           (syntax-line #'section)
                           (syntax-column #'section))
                   #:kind protc-warning  ; TODO
                   #:fix #f])
     ;(println #'(errors ...))
     #'(begin errors ...)]))

(module+ test
  (circular-link no-type (cycle lol1 lol2 lol3)))

(define-syntax (rest stx)
  ; TODO this should produce a warning
  (syntax-warn
   stx
   (syntax-warning
    #:message (format "incomplete parse: ~s")
    #:stx stx
    #:kind #'protc/ur-warning)))

(provide ; FIXME move these all to their own file param.rkt probably
 (prefix-out param: parse-failure)
 (prefix-out param: dimensions)
 (prefix-out param: expr)
 (prefix-out param: quantity)
 (prefix-out param: ratio)
 (prefix-out param: prefix-unit)
 (prefix-out param: unit)
 (prefix-out param: unit-expr))

(define-syntax (parse-failure stx) ; FIXME param: namespace ? also FIXME not a exporting correctly? ; uh does it need to be defined in the kernel?
  (syntax-parse stx
    [(_ body ...)
     #'(quote (param:parse-failure body ...))]))

(define-syntax (quantity stx)
  (syntax-parse stx
    [(~and
      _:sc-quantity
      (_ body ...))
     #:with recurse stx
     #'(begin ; TODO
         (quote recurse)
         body ...)]))

(define-syntax (expr stx)
  (syntax-parse stx
    [_:sc-cur-expr
     #`(quote #,stx)  ; TODO
     ]))

(define-syntax (dimensions stx)
  (syntax-parse stx ; we don't have a standalone syntax class for these, and they are what they are usually
    [(_ body:expr ...)
     #:with recurse stx
     #'(begin ; TODO
         (quote recurse)
         body ...)]))

(define-syntax (prefix-unit stx)
  (syntax-parse stx
    [(_ unit-base:sc-unit-name)
     #`(quote #,stx)  ; TODO
     ]))

(define-syntax (unit stx) ; FIXME required with wrong prefix (i.e. protc: instead of param:) ???
  (syntax-parse stx
    [(_ unit-base:sc-unit-name (~optional unit-prefix:sc-prefix-name))
     #`(quote #,stx)  ; TODO
     ]))

(define-syntax (unit-expr stx) ; FIXME param: protc: confusion here as well
  (syntax-parse stx
    [unit-expr:sc-unit-expr
     #`(quote #,stx)  ; TODO
     ]))

(define-syntax (ratio stx)
  (syntax-parse stx
    [rat:sc-ratio
     #`(quote #,stx)  ; TODO
     ]))

(define-syntax (dilution stx)
  (syntax-parse stx
    [dil:sc-dilution
     #`(quote #,stx)  ; TODO
     ]))
