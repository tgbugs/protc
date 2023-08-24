#lang racket/base
(require racket/string brag/support syntax-color/racket-lexer)
(provide protc-color)

(displayln "protc syntax highlighting alive")

(define-lex-abbrev section (:or "spec" "impl"))
(define-lex-abbrev dots (:or ".uses" ".vars" ".inputs" ".symret"))
(define-lex-abbrev base-executor (:or "lookup" "given" "same" "for-all"))
(define-lex-abbrev activity (:or "measure" "actualize" "make"
                                 "*:" ":*" "**"
                                 ">^>" ">v>" ":>"
                                 ":"
                                 "impl-measure" "impl-actualize" "impl-make"))

(define protc-lexer
  (lexer
   [(eof) (values lexeme 'eof #f #f #f)]
   [dots (values lexeme 'hash-colon-keyword lexeme (pos lexeme-start) (pos lexeme-end))]  ; see Style & Color Names
   [base-executor (values lexeme 'other lexeme (pos lexeme-start) (pos lexeme-end))]
   [section (values lexeme 'keyword lexeme (pos lexeme-start) (pos lexeme-end))]
   [activity (values lexeme 'keyword lexeme (pos lexeme-start) (pos lexeme-end))]
   #;
   [any-char (values lexeme 'error #f (pos lexeme-start) (pos lexeme-end))]))

(define heads (map (λ (s) (string-append s " "))
                   '("spec" "impl"
                     "measure" "actualize" "make"
                     "*:" ":*" "**"
                     ">^>" ">v>" ":>"
                     ":"
                     "impl-measure" "impl-actualize" "impl-make"
                     ".uses" ".vars" ".inputs" ".symret"
                     "lookup" "given" "same" "for-all")))

(define (any-head? peek)
  (for/or ([h heads]) (string-prefix? peek h)))

(define (protc-color port offset mode)
  (cond
    #;
    [(let ([peek (peek-string 1 0 port)])
       (equal? peek "("))
     (define-values (str cat paren start end)
       (racket-lexer port))
     (values str cat paren start end 0 'head-position)]
    [(and (eqv? mode 'head-position)
          (let ([peek (peek-string 10 0 port)])  ; FIXME this is horrible, would be better to try/catch :/
            #; ; extremely verbose debug
            (println peek)
            (and
             (not (eof-object? peek))
             (any-head? peek))))
     (define-values (str cat paren start end)
       (protc-lexer port))
     #; ; debug
     (displayln (list 'color?: str) (current-output-port))
     (define switch-to-racket-mode
       (let ([s0 (string-ref str 0)])
         (or (equal? s0 " ")  ; FIXME :/
             (equal? s0 "\n")
             (equal? s0 "\t")
             (equal? s0 "(")
             (equal? s0 "[")
             (equal? s0 "{"))))
     (values str cat paren start end 0 switch-to-racket-mode)]
    [else
     (define-values (str cat paren start end)
       (racket-lexer port))
     (let ([ccat (if (eof-object? str)
                     'eof ; if you don't set eof and send error on eof drr will error
                     cat
                     #;
                     'error)]
           [mode (if (equal? str "(") 'head-position #t)])
       (values str ccat paren start end 0 mode))]))

(module+ test
  (require (for-syntax racket/base syntax/parse))
  (define-syntax (values->list stx)
    (syntax-parse stx
      [(_ asdf:expr)
       #'(call-with-values (λ () asdf) list)]))
  (values->list (protc-color (open-input-string "") 0 #t))

  (values->list (protc-color (open-input-string "aaa") 0 #t))

  (values->list (protc-color (open-input-string "a") 0 #f))
  (values->list (protc-color (open-input-string "aa") 0 #f))
  (values->list (protc-color (open-input-string "aaa") 0 #f))

  (values->list (protc-color (open-input-string "spec") 0 'head-position))
  (values->list (protc-color (open-input-string "impl") 0 'head-position))

  (values->list (protc-color (open-input-string "make") 0 #f))
  (values->list (protc-color (open-input-string "make") 0 'head-position))
  (values->list (protc-color (open-input-string "measure") 0 #f))
  (values->list (protc-color (open-input-string "actualize") 0 #f))

  (values->list (protc-color (open-input-string " impl") 0 #f))
  (values->list (protc-color (open-input-string "impl ") 0 #f))
  (values->list (protc-color (open-input-string "mpl (") 0 #f))
  (values->list (protc-color (open-input-string "pl (m") 0 #f))
  (values->list (protc-color (open-input-string "l (ma") 0 #f))
  (values->list (protc-color (open-input-string " (mak") 0 #f))

  (values->list (protc-color (open-input-string "impl ") 0 'head-position))
  (values->list (protc-color (open-input-string "implement") 0 #f))

  (values->list (protc-color (open-input-string "(spec") 0 #f))
  (values->list (protc-color (open-input-string "(make") 0 #f))

  (values->list (protc-color (open-input-string "(impl (a b c)) (spec (c d e))") 0 #f))
  )
