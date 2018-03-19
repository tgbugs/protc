#lang racket/base

(provide (all-defined-out))

(define (protc/base-get-info key default default-filter)
  (case key
    ;[(color-lexer) (dynamic-require 'protc/base/colorer 'color)]
    ;[(drracket:indentation) (dynamic-require 'protc/base/indenter 'indent)]
    ;[(drracket:indentation) (indent)]
    [(drracket:toolbar-buttons) (dynamic-require 'protc/protcheck 'button-list)]
    [else (default-filter key default)]))
