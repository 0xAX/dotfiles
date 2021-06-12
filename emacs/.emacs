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
(setq create-lockfiles          nil)
(setq user-emacs-directory      "~/.cache/emacs")

;; set current theme
(setq current-theme "solarized-emacs")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        Package manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(setq package-check-signature 'nil)
(setq package-check-signature nil)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(if (version< emacs-version "27.0")
    (package-initialize))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        Load other extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Load straight.el
;;
;; https://github.com/raxod502/straight.el
;;
(defvar bootstrap-version 5)
(setq package-enable-at-startup nil)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory)))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install and use packages
(straight-use-package 'company)
(straight-use-package 'company-c-headers)
(straight-use-package 'magit)
(straight-use-package 'slime)
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)
(straight-use-package 'yaml)

;; ui
(load "~/.emacscore/file-utils.el")
(load "~/.emacscore/text-utils.el")
(load "~/.emacscore/keybindings.el")
(load "~/.emacscore/org/orgmode.el")
(load "~/.emacscore/ido.el")
(load "~/.emacscore/dired.el")
(load "~/.emacs.d/dash.el/dash.el")
(load "~/.emacscore/ui.el")

;; Development
(load "~/.emacscore/dev/elisp.el")
(load "~/.emacscore/dev/erlang.el")
(load "~/.emacscore/dev/elixir.el")
(load "~/.emacscore/dev/company.el")
(load "~/.emacscore/dev/c.el")
(load "~/.emacscore/dev/common-lisp.el")
(load "~/.emacscore/lisp/cmake.el")
(load "~/.emacscore/term.el")
(load "~/.emacscore/snippets.el")

;; do not use tabs for indentation at all
(setq-default indent-tabs-mode nil)

;; supress all deprecation warnings
(setq warning-minimum-level :emergency)

;; finally loaded everything
(message "All done, %s%s" (user-login-name) ".")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:background "#116599" :foreground "white"))))
 '(tabbar-default ((t (:background "#fdf6e3" :foreground "#eee8d5" :font "Fira Code-12"))))
 '(tabbar-modified ((t (:background "#fdf6e3" :foreground "#d33682"))))
 '(tabbar-selected ((t (:background "#fdf6e3" :foreground "#839496"))))
 '(tabbar-separator ((t (:background "#fdf6e3" :foreground "#fdf6e3"))))
 '(tabbar-unselected ((t (:background "#fdf6e3" :foreground "#93a1a1")))))

(kill-buffer "*straight-process*")
