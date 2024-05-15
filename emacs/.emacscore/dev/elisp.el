;;; elisp.el --- Emacs lisp routines for GNU Emacs  -*- lexical-binding: t -*-

(defun emacs-config ()
  "Open ~/.emacs in new buffer"  
  (interactive)
  (find-file "~/.emacs"))

(defun set-debug-emacs ()
  "Toggle debug-on-error and backtrace-on-error variables"
  (interactive)
  (if (equal debug-on-error nil)
      (progn
        (setq debug-on-error t)
        (setq backtrace-on-error t)
        (message "Debugger is enabled"))
    (progn
        (setq debug-on-error nil)
        (setq backtrace-on-error nil)
        (message "Debugger is disabled"))))
