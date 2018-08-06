#lang racket/base
(require racket/list
         racket/format
         racket/string
         racket/draw
         brag/support
         syntax/strip-context
         protio/parser
         protio/expander
         protio/tokenizer)
(define (doc . expressions)
  expressions)
(define ios '("?" ">"))
(define aspects '(":" "^" "v"))
(define-namespace-anchor anc)
(define ns (namespace-anchor->namespace anc))
(define (sjb s)
  (let* ([expr (string-join s "")]
         [nope (display (~a expr
                            #:width 10
                            #:align 'left))]
         [in-port (open-input-string expr)]
         [parse-tree (parse-to-datum (protio-make-tokenizer in-port))]
         ;[module-syntax (strip-context #`(module protio-module protio/expander #,parse-tree))]
         [words (car (eval parse-tree ns))]

         )
    (display (~a (car words)
                 #:width 40
                 #:align 'left))
    (displayln (cadr words))
    ;(displayln (syntax->datum module-syntax))
    ;(eval module-syntax ns)
    ;(displayln result)
    ;(displayln )
    ;(displayln
    ;(parse-to-datum
    ;(apply-tokenizer-maker
    ;protio-make-tokenizer expr)))
    ;(displayln )
    ))

(define all
  (append
   (map list aspects)
   (cartesian-product ios aspects)
   (cartesian-product aspects ios)
   (cartesian-product ios aspects ios)
   (cartesian-product '(">") aspects '(">'"))))
(define (doit)
  (map sjb all)
  (void))
(doit)
(println (length all))

; draw them
(require racket/class)

(define (tech dc)
  (send dc set-pen "black" 2 'solid)
  (send dc draw-rectangle  ; bounding box
        0 0
        50 50)
  (send dc draw-rectangle
        10 10   ; x y for Top-left
        30 30) ; width height (x y again)
  )

(define (in dc [out #f])
  (send dc set-pen "black" 2 'solid)
  (send dc draw-line
        0 25
        23 25)
  (when (not out)
  (send dc draw-line
        18 20   ; x y for point 1
        23 25)  ; x y for point 2
  (send dc draw-line
        18 30   ; x y for point 1
        23 25)  ; x y for point 2
        ))

(define (out dc)
  (send dc set-pen "black" 2 'solid)
  (send dc draw-line
        25 25
        47 25)
  (send dc draw-line
        42 20
        47 25)
  (send dc draw-line
        42 30
        47 25))

(define (act dc)
  (send dc set-pen "black" 2 'dot)
  (send dc draw-line
        25 0
        25 23)
  (send dc set-pen "black" 2 'solid)
  (send dc draw-line
        20 18
        25 23)
  (send dc draw-line
        30 18
        25 23))

(define (mea dc) ;(TODO dc args #:base [base 60] #:pad [pad 5]) ; need translation and padding
  (send dc set-pen "black" 2 'dot)
  (send dc draw-line
        25 0
        25 23)
  (send dc set-pen "black" 2 'solid)
  (send dc draw-line
        20 7
        25 2)
  (send dc draw-line
        30 7
        25 2))

(define (dot dc)
  (send dc set-brush "black" 'solid)
  (send dc draw-ellipse
        23 23
        4  4
        )

  )

(define target (make-bitmap 50 50)) ; A 30x30 bitmap
(define dc (new bitmap-dc% [bitmap target]))
(define (clear)
  (send dc set-brush "white" 'solid)
  (send dc clear))
(tech dc) (dot dc)
(send target save-file "/tmp/p.png" 'png) (clear)
(tech dc) (in dc) (mea dc)
(send target save-file "/tmp/im.png" 'png) (clear)
(tech dc) (in dc) (act dc)
(send target save-file "/tmp/ia.png" 'png) (clear)
(tech dc) (out dc) (act dc)
(send target save-file "/tmp/ao.png" 'png) (clear)
(tech dc) (in dc #t) (out dc) (mea dc)
(send target save-file "/tmp/imo.png" 'png) (clear)

;(act dc)
;(in dc #t)
;(out dc)

