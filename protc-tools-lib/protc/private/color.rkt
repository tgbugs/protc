#lang racket/base
(require brag/support syntax-color/racket-lexer)
(provide protc-color)

(define-lex-abbrev section (:or "spec" "impl"))
(define-lex-abbrev activity (:or "measure" "actualize" "make"))

(define protc-lexer
  (lexer
   [(eof) (values lexeme 'eof #f #f #f)]
   [section (values lexeme 'keyword lexeme (pos lexeme-start) (pos lexeme-end))]
   [activity (values lexeme 'error lexeme (pos lexeme-start) (pos lexeme-end))]
   #;
   [any-char (values lexeme 'string #f (pos lexeme-start) (pos lexeme-end))]))

(define (protc-color port offset racket-coloring-mode?)
  (cond
    [(let ([peek (peek-string 4 0 port)])
       (or (equal? peek "impl")  ; FIXME ick
           (equal? peek "spec")
           (equal? peek "meas")
           (equal? peek "actu")
           (equal? peek "make")))
     (define-values (str cat paren start end)
       (protc-lexer port))
     ;(displayln (list 'color?: str) (current-output-port))
     (define switch-to-racket-mode
       (let ([s0 (string-ref str 0)])
         (or (equal? s0 " ")  ; FIXME :/
             (equal? s0 "(")
             (equal? s0 "[")
             (equal? s0 "{"))))
     (values str cat paren start end 0 switch-to-racket-mode)]
    [else
     (define-values (str cat paren start end)
       (racket-lexer port))
     (values str cat paren start end 0 #t)]))

(module+ test
  (protc-color (open-input-string "impl") 0 #f)
  (protc-color (open-input-string "spec") 0 #f)
  (protc-color (open-input-string "make") 0 #f)
  (protc-color (open-input-string "measure") 0 #f)
  (protc-color (open-input-string "actualize") 0 #f)
  (protc-color (open-input-string "(impl (a b c)) (spec (c d e))") 0 #f)
  )
