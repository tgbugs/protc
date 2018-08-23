#lang racket/base

(provide (all-defined-out))

(define (protc/base-get-info key default default-filter)
  (case key
    [(color-lexer) (dynamic-require 'protc/private/color 'protc-color)]
    ;[(drracket:indentation) (dynamic-require 'protc/base/indenter 'indent)]
    ;[(drracket:indentation) (indent)]
    [(drracket:toolbar-buttons) (dynamic-require 'protc/protcheck 'button-list)]
    [(drracket:default-filters) '(["Protc Sources" "*.ptc"])]
    [(drracket:default-extension) "rkt"]
    [else (default-filter key default)]))

(module+ test
  ((dynamic-require 'protc/private/color 'protc-color) (open-input-string "hahahhah") #;0)
  (define color (protc/base-get-info 'color-lexer (λ (p o r) "heh") #t))
  (color (open-input-string "a") #;0)
  (color (open-input-string "impl") #;0)
  (color (open-input-string "impl ") #;0)
  (color (open-input-string "impl\n") #;0)
  )
