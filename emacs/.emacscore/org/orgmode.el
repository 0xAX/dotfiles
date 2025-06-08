;;; .emacs --- My org-mode configuration  -*- lexical-binding: t -*-

(require 'org)

;; set path to org directory
(when (file-directory-p "~/dev/todo")
  (setq org-directory "~/dev/todo")
  (setq org-agenda-files '("~/dev/todo")))
(when (file-directory-p "~/disk/dev/todo")
  (setq org-directory "~/disk/dev/todo")
  (setq org-agenda-files '("~/disk/dev/todo")))

;; Add ability to add closing notes to the done items
(setq org-log-done 'time)

;; Enable org-mode for *.org files
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Add ability to follow links by pressing RET
(setq org-return-follows-link  t)

;; Some good defaults
(setq-default
 org-startup-indented t
 line-spacing 1
 org-src-fontify-natively t
 org-fontify-quote-and-verse-block t
 org-pretty-entities t
 org-startup-with-inline-images t
 org-hide-emphasis-markers t
 org-src-tab-acts-natively t
 org-src-preserve-indentation 2
 org-edit-src-content-indentation 2)

;; TODO
(setq org-edit-src-content-indentation 0)
(setq org-src-fontify-natively t
        org-src-window-setup 'current-window ;; edit in current window
        org-src-strip-leading-and-trailing-blank-lines t
        org-src-preserve-indentation t ;; do not put two spaces on the left
        org-src-tab-acts-natively t)


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
(load "~/.emacscore/org/org-markdown.el")

;; Set to nil here, because its incompatible with viewing the agenda
(setq org-startup-with-latex-preview nil)

;; Set latex live preview backend
(setq org-preview-latex-default-process 'dvisvgm)
(setq org-latex-create-formula-image-program 'dvisvgm)

(plist-put org-format-latex-options :scale 2)
(add-hook 'org-mode-hook 'org-fragtog-mode)

(plist-put org-format-latex-options :foreground nil)
(plist-put org-format-latex-options :background nil)
