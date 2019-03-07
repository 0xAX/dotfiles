;;; elisp.el --- Emacs lisp routines for GNU Emacs  -*- lexical-binding: t -*-

(defun emacs-config ()
  "Open ~/.emacs in new buffer"  
  (interactive)
  (find-file "~/.emacs"))
