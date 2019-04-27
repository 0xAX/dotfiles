;;; .emacs --- My init file for GNU Emacs  -*- lexical-binding: t -*-

;; If we are using i3wm, load related configuration
(let*
    ((i3-socket (shell-command-to-string "i3 --get-socketpath"))
     (i3 (file-exists-p (replace-regexp-in-string "\n$" "" i3-socket))))
  (if i3
      (load "~/.emacscore/desktop/i3.el")))

;; Load Standard routines that may be used in other elisp files
(load "~/.emacscore/extensions.el")

;; do not save sessions
(desktop-save-mode 0)
;; Delete text in selection mode when typing
(delete-selection-mode 1)

;; Current locales
(prefer-coding-system        'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
(setq current-language-environment "UTF-8")

;; do not remove new line at the end of buffer
(setq mode-require-final-newline t)

;; Prevent creation of backup files
(setq make-backup-files         nil)
(setq auto-save-list-file-name  nil)
(setq auto-save-default         nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        Package manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(if (version< emacs-version "27.0")
    (package-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        Load other extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ui
(load "~/.emacscore/file-utils.el")
(load "~/.emacscore/text-utils.el")
(load "~/.emacscore/keybindings.el")
(load "~/.emacscore/org/orgmode.el")
(load "~/.emacscore/ido.el")
(load "~/.emacscore/dired.el")
(load "~/.emacscore/ui.el")

;; Development
(load "~/.emacscore/dev/elisp.el")
(load "~/.emacscore/dev/erlang.el")
(load "~/.emacscore/dev/elixir.el")
(load "~/.emacscore/dev/company.el")
(load "~/.emacscore/dev/c.el")
(load "~/.emacscore/dev/golang.el")
(load "~/.emacscore/dev/common-lisp.el")
(load "~/.emacscore/term.el")
(load "~/.emacscore/snippets.el")
(yas-reload-all)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(indent-tabs-mode nil)
 '(package-selected-packages (quote (cider))))

;; finally loaded everything
(message "All done, %s%s" (user-login-name) ".")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :background "#263238" :foreground "CadetBlue1" :box nil :weight bold :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :background "#263238" :foreground "CadetBlue2" :box nil :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :background "#263238" :foreground "CadetBlue3" :height 0.9))))
 '(org-level-4 ((t (:inherit outline-4 :background "#263238" :foreground "#00bfff" :height 0.8))))
 '(org-level-5 ((t (:inherit outline-5 :background "#263238" :foreground "DarkOrange1"))))
 '(org-level-6 ((t (:inherit outline-6 :background "#263238" :foreground "DarkOrange2"))))
 '(org-level-7 ((t (:inherit outline-7 :background "#263238" :foreground "DarkOrange3"))))
 '(org-level-8 ((t (:inherit outline-8 :background "#263238" :foreground "gold2"))))
 '(org-todo ((t (:background "#263238" :foreground "orange red" :weight bold))))
 '(show-paren-match ((t (:background "#116599" :foreground "white")))))
