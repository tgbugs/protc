#lang racket/base
(require racket/string brag/support syntax-color/racket-lexer)
(provide protc-color)

(define-lex-abbrev section (:or "spec" "impl"))
(define-lex-abbrev activity (:or "measure" "actualize" "make"))

(define (protc-color port offset mode)
  (define-values (line col pos) (port-next-location port))
  (define c (read-char port))
  (cond
    [(eof-object? c)
     (values c 'eof #f #f #f 0 mode)]
    [else
     (values (string c)
             (if mode 'error 'error)
             #f
             (+ pos)
             (+ pos 1)
             0
             (not mode))]))

(define protc-lexer
  (lexer
   [(eof) (values lexeme 'eof #f #f #f)]
   [section (values lexeme 'keyword lexeme (pos lexeme-start) (pos lexeme-end))]
   [activity (values lexeme 'keyword lexeme (pos lexeme-start) (pos lexeme-end))]
   #;
   [any-char (values lexeme 'error #f (pos lexeme-start) (pos lexeme-end))]))

#;
(define plog (make-logger 'protc-color #f #;'drracket-language 'info))

#;
(define (protc-color in)
  (log-message plog 'info "currently inside protc-color ...")
  (let-values ([(lexeme type paren start end) (racket-lexer in)])
    (values lexeme 'keyword paren start end)))

#;
(define (protc-color port offset #;racket-coloring-mode?)
  (cond
    [(let ([peek (peek-string 9 0 port)])
       (println peek)
       (or (string-prefix? peek "spec")  ; FIXME ick
           (string-prefix? peek "impl")
           (string-prefix? peek "measure")
           (string-prefix? peek "actualize")
           (string-prefix? peek "make")))
     (define-values (str cat paren start end)
       (protc-lexer port))
     (displayln (list 'color?: str) (current-output-port))
     (define switch-to-racket-mode
       (let ([s0 (string-ref str 0)])
         (or (equal? s0 " ")  ; FIXME :/
             (equal? s0 "\n")
             (equal? s0 "\t")
             (equal? s0 "(")
             (equal? s0 "[")
             (equal? s0 "{"))))
     (values str cat paren start end 0 #;switch-to-racket-mode)]
    [else
     (define-values (str cat paren start end)
       (racket-lexer port))
     (values str 'error paren start end 0 #;#t)]))

(module+ test
  (require (for-syntax racket/base syntax/parse))
  (define-syntax (values->list stx)
    (syntax-parse stx
      [(_ asdf:expr)
       #'(call-with-values (λ () asdf) list)]))

  (values->list (protc-color (open-input-string "a") 0))
  (values->list (protc-color (open-input-string "aa") 0))
  (values->list (protc-color (open-input-string "aaa") 0))

  (values->list (protc-color (open-input-string "spec") 0))
  (values->list (protc-color (open-input-string "impl") 0))

  (values->list (protc-color (open-input-string "make") 0))
  (values->list (protc-color (open-input-string "make") 0))
  (values->list (protc-color (open-input-string "measure") 0))
  (values->list (protc-color (open-input-string "actualize") 0))

  (values->list (protc-color (open-input-string " impl") 0))
  (values->list (protc-color (open-input-string "impl ") 0))
  (values->list (protc-color (open-input-string "mpl (") 0))
  (values->list (protc-color (open-input-string "pl (m") 0))
  (values->list (protc-color (open-input-string "l (ma") 0))
  (values->list (protc-color (open-input-string " (mak") 0))

  (values->list (protc-color (open-input-string "impl ") 0))
  (values->list (protc-color (open-input-string "implement") 0))

  (values->list (protc-color (open-input-string "(spec") 0))
  (values->list (protc-color (open-input-string "(make") 0))

  (values->list (protc-color (open-input-string "(impl (a b c)) (spec (c d e))") 0))
  )
