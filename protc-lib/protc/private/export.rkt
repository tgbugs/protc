#lang racket/base
(require racket/pretty)
(provide protc-export
         protc-export-type)

(define protc-export-type (make-parameter 's-exp))

(define (protc-export protocol)
  (cond [(eq? protc-export-type 's-exp) (protc->s-exp protocol)]
        [(eq? protc-export-type 'html) (protc->html protocol)]
        [(eq? protc-export-type 'tex) (protc->tex protocol)]
        [(eq? protc-export-type 'pdf) (protc->pdf protocol)]
        [#t (error (format "We don't know how to export to ~s. We'd love for you to implement it though! :)" protc-export-type))]))

(define (protc->s-exp protocol) protocol)

(define (protc->html protocol) 'todo)

(define (protc->tex protocol) 'todo)

(define (protc->pdf protocol) 'todo)
