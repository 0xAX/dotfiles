;;; .emacs --- My init file for GNU Emacs  -*- lexical-binding: t -*-

;; If we are using i3wm, load related configuration
(let*
    ((i3-socket (shell-command-to-string "i3 --get-socketpath"))
     (i3 (file-exists-p (replace-regexp-in-string "\n$" "" i3-socket))))
  (if i3
      (load "~/.emacscore/desktop/i3.el")))

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
(setq user-emacs-directory      "~/.emacs.d")

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
(straight-use-package 'counsel)
(straight-use-package 'erlang)
(straight-use-package 'elixir)
(straight-use-package 'ivy)
(straight-use-package 'ivy-posframe)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-treemacs)
(straight-use-package 'magit)
(straight-use-package 'markdown-mode)
(straight-use-package 'org-bullets)
(straight-use-package 'pkg-info)
(straight-use-package 'rust-mode)
(straight-use-package 'solarized-emacs)
(straight-use-package 'slime)
(straight-use-package 'slime-company)
(straight-use-package 'swiper)
(straight-use-package 'tabbar)
(straight-use-package 'yaml-mode)
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)

;; ui
(load "~/.emacscore/system.el")
(load "~/.emacscore/file-utils.el")
(load "~/.emacscore/text-utils.el")
(load "~/.emacscore/keybindings.el")
(load "~/.emacscore/org/orgmode.el")
(load "~/.emacscore/ido.el")
(load "~/.emacscore/dired.el")
(load "~/.emacscore/ui.el")
(load "~/.emacscore/vcs/magit.el")

;; Development
(load "~/.emacscore/dev/lsp-mode.el")
(load "~/.emacscore/dev/sed.el")
(load "~/.emacscore/dev/shell.el")
(load "~/.emacscore/dev/elisp.el")
(load "~/.emacscore/dev/erlang.el")
(load "~/.emacscore/dev/elixir.el")
(load "~/.emacscore/dev/company.el")
(load "~/.emacscore/dev/c.el")
(load "~/.emacscore/dev/common-lisp.el")

;; Load miscellaneous things
(load "~/.emacscore/term.el")
(load "~/.emacscore/snippets.el")
(load "~/.emacscore/build/make.el")
(load "~/.emacscore/markups.el")
(load "~/.emacscore/dotfiles.el")

;; load 1 file things
(load "~/.emacscore/lisp/cmake.el")
(load "~/.emacscore/lisp/rainbow-mode-1.0.5.el")

;; do not use tabs for indentation at all
(setq-default indent-tabs-mode nil)

;; supress all deprecation warnings
(setq warning-minimum-level -1)

;; kill unneded buffers
(when (get-buffer "*straight-process*")
  (kill-buffer "*straight-process*"))
(when (get-buffer "*straight-byte-compilation*")
  (kill-buffer "*straight-byte-compilation*"))

;; finally loaded everything
(message "All done, %s%s" (user-login-name) ".")
