#lang racket/base
(require racket/string
         racket/list
         ;"private/export.rkt"
         protc/export
         )

;(define (export . rest) "TODO")

; TODO  #lang docopt -> (require "export-options.rkt") -> hash of the options + the docstring

(let* ([cmdline (vector->list (current-command-line-arguments))]
       ; TODO multiple -f statements
       [lc (length cmdline)]
       ;[x (println cmdline)]
       ;[x (println lc)]
       [-format (cond [(null? cmdline) #f]
                      [(or (equal? (car cmdline) "-f")
                           (equal? (car cmdline) "--format"))
                       (if (> 2 lc)
                           #f
                           (string->symbol (cadr cmdline)))]
                      [else #f])]
       ;[x (println -format)]
       [files (cond [-format (drop cmdline 2)]
                    [(null? cmdline) cmdline]
                    [else
                     (if (or (equal? (car cmdline) "-f")
                             (equal? (car cmdline) "--format"))
                         null
                         cmdline)])]
       [format (if -format -format 'html)]
       ;[x (println format)]
       ;[x (println files)]
       )
  (if (null? files)
      (eprintf "Usage:
raco protc export [option ...] <file> ...
  Exports protocol <file> ... to an executable format.
 where option is one of
  -f <format>,  --format <format> : the export format [default: html]
")
      (for ([file files])
        (export-file file #:format format))))
