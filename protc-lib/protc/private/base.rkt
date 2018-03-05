#lang racket/base
(require racket/syntax syntax/parse)
;;; implementation of protc forms using only racket syntax tools

;; new syntax classes that we are defining for protc
; used to enforce section ordering and structure during pattern matching
; see https://docs.racket-lang.org/syntax/stxparse-specifying.html#(part._stxparse-attrs)

;; section
#'(name:section-name header:section-header body:messages)
;; section-header probably want to break beings into their own thing...
; as it exists now
#'((or *being* *aspect aspect* aspect) variable-name:id granular-imports)
;; body:messages
; do we need to be able to make defining the accepted value types for messages part of the language?
; does single-expression in the context evaluate if not quoted? need to test, also values
#'(message-name:message-type message-value:single-expression)
#'(invariant thing:defined-in-inputs-or-imports aspect-name:defined-aspect [message-name:message-type [message-value]+]+)

;; defining the symbolic components
; Translates nicely to using def for the purely symbolic parts
; eg
; def :aspect-name words if a given, otherwise operations on other aspects or a spec section combining...
; def .message-name
; def *being-name*
; def function  ; juse reuse define...
(define-syntax (define-black-box stx) stx)
(define-syntax (define-aspect stx)
  (syntax-parse stx
    [(define-aspect aspect-name:id body:expr)
     ()
     ]
    ))
(define-syntax (define-message stx) stx)

; *aspect says I'm going to measure this (measure aspect)
; aspect* says I'm going to actualize this (actualize aspect)
; aspect says I'm going to define this based on other aspects (define aspect)
;  or define it as a fundamental quantity aspect, id something like the kilogram
;  all other actual measurement devices used in *aspect sections

(define-syntax-class section-header
  (pattern ))

(define-syntax (spec stx)
  (syntax-parse stx
    [(spec () )]))













