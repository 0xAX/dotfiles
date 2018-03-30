;;; go.el --- Golang routines for GNU Emacs  -*- lexical-binding: t -*-
;;
;; Author:  Alexander Kuleshov <kuleshovmail@gmail.com>
;; URL:     https://github.com/0xAX/med
;;
;; License: See LICENSE file.

;; Load go-mode
(add-to-list 'load-path "~/.emacs.d/go-mode")
(require 'go-mode)

;; Add some go fmt before save
(add-hook 'before-save-hook 'gofmt-before-save)
