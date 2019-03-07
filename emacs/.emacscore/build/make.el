;;; make.el --- make(1) routines for GNU Emacs  -*- lexical-binding: t -*-

(require 'make-mode)

(defconst makefile-nmake-statements
  `("!IF" "!ELSEIF" "!ELSE" "!ENDIF" "!MESSAGE" "!ERROR" "!INCLUDE" ,@makefile-statements)
  "List of keywords understood by nmake.")

(defconst makefile-nmake-font-lock-keywords
  (makefile-make-font-lock-keywords
   makefile-var-use-regex
   makefile-nmake-statements
t))

(define-derived-mode makefile-nmake-mode makefile-mode "nMakefile"
  "An adapted `makefile-mode' that knows about nmake."
  (setq font-lock-defaults
    `(makefile-nmake-font-lock-keywords ,@(cdr font-lock-defaults))))
