#lang racket/base

(provide (all-defined-out))

(define (protc/base-get-info key default default-filter)
  (case key
    [(color-lexer) (dynamic-require 'protc/base/color 'protc-color)]
    ;[(drracket:indentation) (dynamic-require 'protc/base/indenter 'indent)]
    ;[(drracket:indentation) (indent)]
    [(drracket:toolbar-buttons) (dynamic-require 'protc/protcheck 'button-list)]
    [(drracket:default-filters) '(["Protc Sources" "*.ptc"])]
    [(drracket:default-extension) "rkt"]
    [else (default-filter key default)]))
