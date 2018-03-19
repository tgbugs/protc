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

; from https://docs.racket-lang.org/tools/implementing-tools.html
(define (reverse-content text)
  (for ((x (in-range 1 (send text last-position))))
    (send text split-snip x))
  (define snips
    (let loop ((snip (send text find-first-snip)))
      (if snip
          (cons snip (loop (send snip next)))
          '())))
  (define released-snips
    (for/list ((snip (in-list snips))
               #:when (send snip release-from-owner))
      snip))
  (for ((x (in-list released-snips)))
    (send text insert x 0 0)))

(define (protcheck-func frame)
  (define syntax-checking "I'm checking syntax I swear.")
  (define definitions-text (send frame get-definitions-text))
  ;(reverse-content definitions-text)  ; lol
  (send definitions-text insert syntax-checking)
  (define position (send definitions-text get-start-position))
  (send definitions-text set-position (- position (string-length syntax-checking)))
  )



(define protcheck-button
  (list
   protcheck-button-label 
   protcheck-bitmap
   protcheck-func
   #f))

(define button-list
  (list protcheck-button))






