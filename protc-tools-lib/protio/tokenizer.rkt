#lang racket/base
(require brag/support)
(provide protio-make-tokenizer)

(define-lex-abbrev newlines (:+ (char-set "\xA\xD")))

(define (protio-make-tokenizer port)
  (define (next-token)
    (define protio-lexer
      (lexer-srcloc
       [(eof) (return-without-srcloc eof)]
       [(from/to ";" newlines) (token 'COMMENT)]
       [whitespace (token 'WS lexeme)]
       ["(" (token 'L-PAREN lexeme)]
       [")" (token 'R-PAREN lexeme)]
       ["1" (token 'PRIMARY lexeme)]  ; TODO
       ["?" (token 'UNSPECIFIED lexeme)]
       [">'" (token 'OUTPUT-PRIME lexeme)]
       [">" (token 'INPUT-OUTPUT lexeme)]
       [(:or "." ":" "|") (token 'BAR lexeme)]
       ["^" (token 'MEASURE lexeme)]
       ["v" (token 'ACTUALIZE lexeme)]
       [(:or "dual" "<->") (token 'DUAL 'dual)]
       [(:or "chain" "->") (token 'CHAIN 'chain)]
       ))
    (protio-lexer port))
  next-token)
