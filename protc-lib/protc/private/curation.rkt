#lang racket/base

(require
 "direct-model.rkt"
 (for-syntax
  racket/base
  racket/syntax
  syntax/parse
  "utils.rkt"
  ;"direct-model.rkt"
  "syntax-classes.rkt"))

(provide (all-defined-out)
         (all-from-out "direct-model.rkt"))

;;; identifier namespaces  TODO centralize this ...

(define (hyp: id)
  ; TODO struct?
  (string-append "https://hyp.is/" (symbol->string id)))

;;;

; aspect and input are the only 2 that need to lift units out

(define-syntax (actualize stx)
  (syntax-parse stx
    [thing #;(_ wut:id (~optional (~seq #:prov prov)) rest:expr ...)
     #''thing]))

(define-syntax (input stx)
  (syntax-parse stx
    #:datum-literals (hyp:)
    #;
    (aspect protc:aspct
            input protc:input
            parameter* protc:parameter*
            invariant protc:invariant)
    [(_ name:str (hyp: (quote id))
        (~alt ;inv/par:sc-cur-inv/par
         inv:sc-cur-invariant
         par:sc-cur-parameter*
         body:expr) ...)
     #:with black-box (datum->syntax #'name (string->symbol (syntax-e #'name)))
     #'(actualize black-box #:prov (hyp: 'id)
                  (~? inv.lifted) ...
                  (~? par.lifted) ...
                  (~? body (raise-syntax-error "HOW?!")) ...
               )
     ]))

(define-syntax (output stx)
  (syntax-parse stx
    #:datum-literals (hyp: quote)
    [(_ name:str (hyp: (quote id)) (~alt asp:sc-cur-aspect par:sc-cur-parameter* input:expr) ...)
     #:with spec-name #'id
     #:with black-box (datum->syntax #'name (string->symbol (syntax-e #'name)))
     #'(define-make (spec-name black-box)
         (.inputs input ...)
         (.outputs (: black-box (~? asp) ... (~? par) ...))
         )
     ]))

(define (black-box-component name prov . aspects) #f)

(define (aspect aspect/unit/unit-exp) #f)

(define-syntax (unit stx)
  (syntax-parse stx
    [(_ unit-base:sc-unit-name (~optional unit-prefix:sc-prefix-name))
     #`(quote #,stx)  ; TODO
     ]))

(define-syntax (unit-expr stx)
  (syntax-parse stx
    [(_ unit-expr:sc-unit-expr)
     #`(quote #,stx)  ; TODO
     ]))

(define (invariant aspect value prov) #f)
(define (parameter* aspect value prov) #f)
(define (objective* text prov) #f)
(define (telos text prov) #f)
(define (result aspect value prov) #f)

(define (order) #f)
(define (repeate) #f)

(define (references-for-use) #f)
(define (references-for-evidence) #f)

(define (black-box) #f)

(module+ test
  (require rackunit
           "utils.rkt"
           "syntax-classes.rkt"
           syntax/parse
           racket/function)
  (define-syntax (test stx)
    (syntax-parse stx
      [(_ thing:sc-cur-invariant)
       #''thing.lifted]))

  #;
  (define-syntax (thunk stx)
    #`(λ () #,(cdr (syntax->list stx))))

  #; ; i have no idea why this doesn't work
  (check-exn exn:fail:syntax? (thunk (test (parameter* (hyp: '0) (quantity 10 (unit 'meters))))))

  (test (invariant (hyp: '0) (quantity 10 (unit meters))))

  (input "thing" (hyp: '1) (parameter* (hyp: '2) (quantity 10 (unit meters))))

  (define-syntax (unit-> stx)
    (syntax-parse stx
      [(_ name:expr prefix:expr)
       #:with n (fmtid "~a" (datum->syntax #'name (syntax-local-eval #'name)))
       #:with p (fmtid "~a" (datum->syntax #'prefix (syntax-local-eval #'prefix)))
       #'(unit n p)]
      ))

  (define (runtime-unit name prefix)
    (with-syntax ([n name]
                  [p prefix])
      (syntax-parse #'(unit n p)
        [thing:sc-unit
         (syntax->datum #'thing)])))

  (unit meters milli)
  (unit 'meters 'milli)  ; not entirely sure if we want this ... but ok
  (unit-> 'meters 'milli)
  #; ; another cases where I think I need to trap the error as I do in rrid-metadata
  ; however, the fact that syntax-local-eval fails to find thunk and (unit a b) needs
  ; identifiers means that the runtime-unit implementation is vastly preferable
  (check-exn exn:fail:syntax:unbound? (thunk (unit-> ((thunk 'meters)) ((thunk 'milli)))))
  (runtime-unit ((thunk 'meters)) ((thunk 'milli)))
  (check-exn exn:fail:syntax? (thunk (runtime-unit 'nota 'unit)))
  (let ([m 'meters]
        [_m 'milli])
    (runtime-unit m _m)))
