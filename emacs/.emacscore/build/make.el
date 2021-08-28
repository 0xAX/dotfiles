;;; make.el --- make(1) routines for GNU Emacs  -*- lexical-binding: t -*-

(require 'make-mode)

;; special hook to fix cua-mode in makefiles
;;
;; NOTE: not really sure why but when I am openning Makefile/GNUMakefile
;; in my GNU Emacs - Cua mode is broken by some reasons. Re-enabling
;; helps to fix it.
(add-hook 'makefile-mode-hook
          (lambda () (progn
                       (cua-mode nil)
                       (cua-mode t))))
