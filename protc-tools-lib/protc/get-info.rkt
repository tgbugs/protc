#lang racket/base

(provide protc/base-get-info)

(define (protc/base-get-info key default default-filter)
  (case key
    [(color-lexer) (dynamic-require 'protc/syntax-hl 'protc-color)]
    ;[(drracket:indentation) (dynamic-require 'protc/base/indenter 'indent)]
    ;[(drracket:indentation) (indent)]
    [(drracket:toolbar-buttons) (dynamic-require 'protc/protcheck 'button-list)]
    [(drracket:default-filters) '(["Protc Sources" "*.ptc"])]
    [(drracket:default-extension) "rkt"]
    [else (default-filter key default)]))

(module+ test
  ((dynamic-require 'protc/private/color 'protc-color) (open-input-string "hahahhah") 0 #t)
  (define color (protc/base-get-info 'color-lexer (λ (p o r) "heh") #t))
  (color (open-input-string "a") 0 #t)
  (color (open-input-string "impl") 0 #t)
  (color (open-input-string "impl ") 0 #t)
  (color (open-input-string "impl\n") 0 #t)
  )
