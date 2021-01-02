;;; common-lisp.el --- SLIME configuration -*- lexical-binding: t -*-

;; load SLIME
(add-to-list 'load-path "~/.emacs.d/slime")
(require 'slime-autoloads)

;; setup path to lisp compiler
(setq inferior-lisp-program "/usr/bin/sbcl")
