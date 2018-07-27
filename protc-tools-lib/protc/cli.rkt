#lang racket/base
(require raco/command-name)

(let* ([cmdline (vector->list (current-command-line-arguments))]
       [cmd (if (null? cmdline)
                cmdline
                (car cmdline))]
       [mod (cond [(equal? cmd "check") 'protc/check/cli]
                   [(equal? cmd "export") 'protc/export/cli]
                   [else #f])]
       )
  (if mod
      (parameterize ([current-command-line-arguments (list->vector (cdr cmdline))]
                     [current-command-name cmd])
        (dynamic-require mod #f)) 
      (eprintf "Usage:
raco protc <subcommand> [option ...] <arg ...>

subcommands
  raco protc check            check protocol for consistency and correctness
  raco protc export           export protocols to executable formats
")))
