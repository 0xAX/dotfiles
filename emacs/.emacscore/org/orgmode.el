;;; .emacs --- My org-mode configuration  -*- lexical-binding: t -*-

(require 'org)

;; set path to org directory
(when (file-directory-p "~/dev/todo")
  (setq org-directory "~/dev/todo"))
(when (file-directory-p "~/disk/dev/todo")
  (setq org-directory "~/disk/dev/todo"))

;; Add ability to add closing notes to the done items
(setq org-log-done 'note)

;; enable org-mode for *.org files
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Some good defaults
(setq-default
 org-startup-indented t
 line-spacing 1
 org-src-fontify-natively t
 org-fontify-quote-and-verse-block t
 org-pretty-entities t
 org-startup-with-inline-images t
 org-hide-emphasis-markers t
 org-adapt-indentation nil
 org-src-preserve-indentation nil
 org-edit-src-content-indentation 0)

;; Enable auto-search in org-mode so any non-standard keypress in C-x C-j
;; mode will lead to search according to pressed keys
(setq org-goto-auto-isearch t)

;; Enable pretty printing of special symbols
(org-toggle-pretty-entities)

;; Enable support for org-tables everywhere
(add-hook 'message-mode-hook 'turn-on-orgtbl)

;; load additional org-mode helpers
(load "~/.emacscore/org/org-api.el")
(load "~/.emacscore/org/org-babel.el")
(load "~/.emacscore/org/org-ui.el")
(load "~/.emacscore/org/org-keybindings.el")
