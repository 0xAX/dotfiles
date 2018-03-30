;;; elisp.el --- Emacs lisp routines for GNU Emacs  -*- lexical-binding: t -*-
;;
;; Author:  Alexander Kuleshov <kuleshovmail@gmail.com>
;; URL:     https://github.com/0xAX/med
;;
;; License: See LICENSE file.

(defun emacs-config ()
  "Open ~/.emacs in new buffer"  
  (interactive)
  (find-file "~/.emacs"))
