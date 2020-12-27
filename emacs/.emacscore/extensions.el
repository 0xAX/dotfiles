;;; utils.el --- Auxiliary functions for GNU Emacs  -*- lexical-binding: t -*-

(defun load-additional-modes ()
  "Load additional modes (web, and etc....)"  
  (interactive)
  (load "~/.emacscore/lisp/rainbow-mode-1.0.5.el")
  (rainbow-mode)
  (load "~/.emacscore/dev/sed.el")
  (load "~/.emacscore/dev/shell.el")
  ;;
  ;; YAML mode
  ;;
  (add-to-list 'load-path "~/.emacs.d/yaml")
  (require 'yaml-mode)
  ;;
  ;; some build/term and text extensions
  ;;
  (load "~/.emacscore/build/make.el")
  (load "~/.emacscore/markups.el"))
