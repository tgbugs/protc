#lang racket/base

; saved once again by https://beautifulracket.com/jsonic-2/toolbar-buttons.html
; ultimately this should be implemented as a tool@ as is being worked on
; elsewhere in the repo, but getting the basic functionality up first is a priority

(require racket/class
         images/compile-time
         (for-syntax racket/base images/icons/tool images/icons/style)
         )

(provide button-list)

(define protcheck-button-label "Protocol Checker")

; from syncheck-drracket-button.rkt
(define protcheck-bitmap
  (compiled-bitmap (check-syntax-icon #:height (toolbar-icon-height))))
(define protcheck-small-bitmap
  (compiled-bitmap (small-check-syntax-icon #:height (toolbar-icon-height))))

(define (protcheck-func frame)
  (define syntax-checking "I'm checking syntax I swear.")
  (define definitions-text (send frame get-definitions-text))
  (send definitions-text insert syntax-checking)
  (define position (send definitions-text get-start-position))
  (send definitions-text set-position (- position (string-length syntax-checking))))

(define protcheck-button
  (list
   protcheck-button-label 
   protcheck-bitmap
   protcheck-func
   #f))

(define button-list
  (list protcheck-button))






