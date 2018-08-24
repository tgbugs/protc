#lang racket/base

(require (for-syntax "syntax-classes.rkt" syntax/parse))

(define (input name prov . aspects) #f)
(define (output name prov . aspects) #f)
(define (black-box-component name prov . aspects) #f)

(define (aspect aspect/unit/unit-exp) #f)

(define-syntax (unit stx)
  (syntax-parse stx
    [(_ unit-base:sc-unit (~optional unit-prefix:sc-prefix))
     stx  ; TODO
     ]))
(define-syntax (unit-expr stx)
  (syntax-parse stx
    [(_ unit-expr:sc-unit-expr)
     stx  ; TODO
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

