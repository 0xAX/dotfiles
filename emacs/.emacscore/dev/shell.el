;;; sed.el --- Bash/Shell routines for GNU Emacs  -*- lexical-binding: t -*-
;;
;; Author:  Alexander Kuleshov <kuleshovmail@gmail.com>
;; URL:     https://github.com/0xAX/med
;;
;; License: See LICENSE file.

(defun shell-indentation ()
  (interactive)
  (setq sh-basic-offset 4
        sh-indentation  4
        sh-indent-for-case-label 0
        sh-indent-for-case-alt '+))

(add-hook 'sh-mode-hook 'shell-indentation)
