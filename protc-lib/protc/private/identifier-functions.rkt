#lang racket/base

(provide (all-defined-out))

;;; identifier namespaces

(define (hyp: id)
  ; TODO struct?
  (string-append "https://hyp.is/" (symbol->string id)))

(define (DOI: id)
  (string-append "https://doi.org/"
                 (if (symbol? id)
                     (symbol->string id)
                     id)))

(define (PMID: id)
  (string-append "https://www.ncbi.nlm.nih.gov/pubmed/"))

(module+ test
  (PMID: 1234567)
  (DOI: '10.1145/367177.367199)
  (hyp: 'yrnwoguZEeeIFfvQZd6Alg)
  )
