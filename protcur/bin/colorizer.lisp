#!/usr/bin/sbcl --script
;(write-line "HELP IM TRAPPED IN A REPL")
(load "~/.sbclrc")  ; assuming that quicklisp-init is pulled in ...
;(pprint (list-all-packages))
;(write-line "")
(require "colorize")
(if (> (length sb-ext:*posix-argv*) 1)
    (progn
      (write-line "ok")
      (colorize:colorize-file :scheme (cadr sb-ext:*posix-argv*))
      (exit :code 0))
    (progn (write-line "not ok")
           (exit :code 1)))
