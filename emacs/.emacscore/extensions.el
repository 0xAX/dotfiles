;;; utils.el --- Auxiliary functions for GNU Emacs  -*- lexical-binding: t -*-
;;
;; Author:  Alexander Kuleshov <kuleshovmail@gmail.com>
;; URL:     https://github.com/0xAX/med
;;
;; License: See LICENSE file.

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
