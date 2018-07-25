#lang info

(define raco-commands
  (list (list "protc-check"
              'protc/check/cli
              "runs correctness and completeness checks for Protc protocols"
              #f)
        (list "protc-export"  ; FIXME want protc export, protc check etc...
              'protc/export/cli
              "export Protc protocols to executable formats"
              #f)))
