;;; magit.el --- magit configuration  -*- lexical-binding: t -*-
;;
;; Author:  Alexander Kuleshov <kuleshovmail@gmail.com>
;; URL:     https://github.com/0xAX/med
;;
;; License: See LICENSE file.

(add-to-list 'load-path "~/.emacs.d/dash.el")
(add-to-list 'load-path "~/.emacs.d/with-editor")
(add-to-list 'load-path "~/.emacs.d/magit/lisp")
(require 'magit)

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/.emacs.d/magit/Documentation/"))
