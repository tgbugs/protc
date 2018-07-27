#lang racket/base

(define (check file)
  (displayln "Doesn't look like anything to me."))

(let* ([cmdline (vector->list (current-command-line-arguments))]
       #;[cmd (if (null? cmdline)
                cmdline
                (car cmdline))]
       )
  (cond [(null? cmdline)
         (eprintf "Usage:
raco protc check [option ...] <file> ...
  Runs the protocol checker on <file> ...
")]
        [else (for-each check cmdline)]))
