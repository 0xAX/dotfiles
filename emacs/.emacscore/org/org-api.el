;;; .emacs --- My org-mode configuration  -*- lexical-binding: t -*-

(defun todo()
  "Open TODO directory in dired mode"
  (interactive)
  (dired org-directory))
