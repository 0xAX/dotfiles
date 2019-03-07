;;; utils.el --- Auxiliary functions for GNU Emacs  -*- lexical-binding: t -*-

(defun load-additional-dev-modes ()
  "Load additional development modes (web, and etc....)"  
  (interactive)
  (load "~/.emacscore/dev/sed.el")
  (load "~/.emacscore/dev/shell.el")
  (load "~/.emacscore/dev/rust.el")
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
