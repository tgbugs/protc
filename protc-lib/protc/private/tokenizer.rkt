#lang racket/base
(require
 brag/support
 (rename-in
  br-parser-tools/lex-sre
  (- :-))

 )
(define-lex-abbrev digits (:+ (char-set "0123456789")))
(define-lex-abbrev number (:: digits
                              (? (:: (? (:: "." digits))
                                     (? (:: (char-set "Ee") digits))))))
;(define-lex-abbrev float (:: digits "." digits))
;(define-lex-abbrev scinot (:: (:or float digits) (char-set "Ee") (:or float digits)))
;(define-lex-abbrev code-delimiters (:or ">>>" "<<<"))
;(define-lex-abbrev code-block (intersection (complement "<<<") (:+ any-char)))
;(define-lex-abbrev reserved-words code-delimiters)
(define-lex-abbrev symbol-chars (:+ (:or alphabetic symbolic numeric (char-set "-*_&%#!?.:/"))))
;(define-lex-abbrev symbol-set (intersection (complement reserved-words) symbol-chars))
(define-lex-abbrev sections (:or "spec" "act" "impl"))
(define (protc-make-tokenizer port)
  (define (next-token)
    (define protc-lexer
      (lexer-srcloc
       ; order matters inside of here
       [(eof) (return-without-srcloc eof)]
       ;[whitespace (next-token)]
       [whitespace (token lexeme #:skip? #t)]
       [(from/to "#|" "|#") (token 'COMMENT-ML #:skip? #t)]
       [(from/to ";" "\n") (token 'COMMENT  ; TODO put the comments in their own place
                                  (substring lexeme 1
                                             (sub1 (string-length lexeme))) #:skip? #t)]
       [number (token 'NUMBER (string->number lexeme))]  ; TODO octal and hex
       [(from/to "\"" "\"") (token 'STRING
                                   (substring lexeme 1
                                              (sub1 (string-length lexeme))))]
       [(: "." symbol-chars) (token 'MESSAGE (substring lexeme 1))]
       [(: "*" symbol-chars "*") (token 'BEING (substring lexeme 1
                                                          (sub1 (string-length lexeme))))]
       [(: ":*" symbol-chars) (token 'ASPECT-PARAM (substring lexeme 2))]
       [(: ":" symbol-chars "*") (token 'ASPECT-MEASURE
                                        (substring lexeme 1
                                                   (sub1 (string-length lexeme))))]
       [(: ":" symbol-chars) (token 'ASPECT (substring lexeme 1))]
       ; TODO symbolizable vs non-symbolizable eg mouse :brain is not a proper aspect but rather a nested black box or something ... so we may need mouse <brain <cortex *mouse* <*brain* <*cortex* seems pretty darned verbose....
       ["." (token 'END-SECTION)]
       ["..." (token 'ELIPSIS)]
       [symbol-chars (token 'SYMBOL (string->symbol lexeme))] ; FIXME too greedy
       [sections (token 'SECTION lexeme)]  ; this allows definition of new section types
       ["(" (token 'OPEN)]
       [")" (token 'CLOSE)]
       ;["@(" (token 'OPEN-AEXP)]
       [(from/to "@(" ")@") (token 'AEXP lexeme)]  ; FIXME
       ;[(from/to "(" ")") (token 'SEXP lexeme)]
       ["'" (token 'QUOTE)]
       ["," (token 'COMMA)]
       [(from/to ">>>" "<<<") (token 'CODE-BLOCK
                                     (substring lexeme 3
                                                (- (string-length lexeme) 3)))]
       ))
    (protc-lexer port))
  next-token)
(provide protc-make-tokenizer)
