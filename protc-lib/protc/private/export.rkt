#lang racket/base
(require
 racket/pretty
 protc/export
         )
(provide protc-export
         protc-export-type)

(define protc-export-type (make-parameter 's-exp))

(define (protc-export protocol)
  (let ([protc-export-type (protc-export-type)])
    (println (list 'pet: protc-export-type))
    (cond [(eq? protc-export-type 's-exp) (protc->s-exp protocol)]
          [(eq? protc-export-type 'html) (protc->html protocol)]
          [(eq? protc-export-type 'html-live) (protc->html-live protocol)]
          [(eq? protc-export-type 'tex) (protc->tex protocol)]
          [(eq? protc-export-type 'pdf) (protc->pdf protocol)]
          [#t (error (format "We don't know how to export to ~s. We'd love for you to implement it though! :)" protc-export-type))])))

(define (protc->s-exp protocol) protocol)

(define (protc->scrib protocol)
  (let-values ([(name scrib) (protc->scribble protocol)])
    scrib))

(define (protc->html protocol)
  (scribble->html (protc->scrib protocol)))

(define (protc->html-live protocol)
  ; render only the html, assume that supporting files will already be present
  (ast->html protocol))

(define (protc->tex protocol)
  (scribble->tex (protc->scrib protocol)))

(define (protc->pdf protocol)
  (scribble->pdf (protc->scrib protocol)))
