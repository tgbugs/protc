#lang info

(define raco-commands
  (list (list "check-protocol"
              'protc/protcheck/cli
              "runs correctness and completeness checks for Protc protocols"
              #f)))
