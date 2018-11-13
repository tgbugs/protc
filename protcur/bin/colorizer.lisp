#!/usr/bin/sbcl --script
;(write-line "HELP IM TRAPPED IN A REPL")
(let ((quicklisp-init (merge-pathnames "code/lisp/quicklisp/setup.lisp"
                                       (user-homedir-pathname))))
  (when (probe-file quicklisp-init)
    (load quicklisp-init)))
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
