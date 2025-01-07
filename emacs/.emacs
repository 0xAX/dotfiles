;;; .emacs --- My init file for GNU Emacs  -*- lexical-binding: t -*-

;; Set directory for dependencies and primary configuration
(setq user-emacs-directory "~/.emacs.d")
;; Set directory with secondary configuration
(setq user-emacs-configuration-directory "~/.emacscore")

;; If we are using i3wm, load related configuration.
(if (executable-find "i3")
  (let*
      ((i3-socket (shell-command-to-string "i3 --get-socketpath"))
       (i3 (file-exists-p (replace-regexp-in-string "\n$" "" i3-socket))))
    (if i3
        (progn
          (load "~/.emacscore/desktop/i3.el")
          (setq *i3* "true"))
      (setq *i3* "false")))
  (setq *i3* "false"))

;; Kill buffers without asking confirmation about active running processes
(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; do not save sessions
(desktop-save-mode 0)
;; Delete text in selection mode when typing
(delete-selection-mode 1)

;; Set character sets
(prefer-coding-system        'utf-8)
;; Set coding system of terminal output
(set-terminal-coding-system  'utf-8)
;; Set codign system for keyboard input
(set-keyboard-coding-system  'utf-8)
;; Set coding system to communicate with other X systems
(set-selection-coding-system 'utf-8)
;; Set coding system to use with system messages
(setq locale-coding-system   'utf-8)
;; Set language environment
(setq current-language-environment "UTF-8")

;; Prevent creation of backup files
(setq make-backup-files         nil)
(setq auto-save-list-file-name  nil)
(setq auto-save-default         nil)
(setq create-lockfiles          nil)

(require 'package)
(setq package-check-signature 'nil)
(setq package-check-signature nil)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(if (version< emacs-version "27.0")
    (package-initialize))

;;
;; Load straight.el
;;
;; https://github.com/raxod502/straight.el
;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install and use packages
(straight-use-package 'auctex)
(straight-use-package 'bison-mode)
(straight-use-package 'bnf-mode)
(straight-use-package 'catppuccin-theme)
(straight-use-package 'company)
(straight-use-package 'company-c-headers)
(straight-use-package 'counsel)
(straight-use-package 'erlang)
(straight-use-package 'elixir)
(straight-use-package 'go-mode)
(straight-use-package 'ivy)
(straight-use-package 'ivy-posframe)
(straight-use-package 'julia-mode)
(straight-use-package 'julia-repl)
(straight-use-package 'lsp-mode)
(straight-use-package 'lsp-ui)
(straight-use-package 'lsp-treemacs)
(straight-use-package 'lua-mode)
(straight-use-package 'magit)
(straight-use-package 'markdown-mode)
(straight-use-package 'material-theme)
(straight-use-package 'org-bullets)
(straight-use-package 'org-fragtog)
(straight-use-package 'pkg-info)
(straight-use-package 'rust-mode)
(straight-use-package 'rustic)
(straight-use-package 'solarized-emacs)
(straight-use-package 'slime)
(straight-use-package 'slime-company)
(straight-use-package 'swiper)
(straight-use-package 'tabbar)
(straight-use-package 'vterm)
(straight-use-package 'yaml-mode)
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)
(straight-use-package 'zig-mode)

;; Load the small libraries first
(load "~/.emacscore/lisp-utils.el")
(load "~/.emacscore/lisp/org-bullets.el")
(load "~/.emacscore/lisp/cmake.el")
(load "~/.emacscore/lisp/rainbow-mode-1.0.5.el")
(load "~/.emacscore/lisp/org-bullets.el")
(load "~/.emacscore/lisp/org-fragtog.el")
(load "~/.emacscore/lisp/epl.el")
(load "~/.emacscore/lisp/pkg-info.el")

;; Emacs UI and utils
(load "~/.emacscore/system.el")
(load "~/.emacscore/file-utils.el")
(load "~/.emacscore/text-utils.el")
(load "~/.emacscore/keybindings.el")
(load "~/.emacscore/ido.el")
(load "~/.emacscore/dired.el")
(load "~/.emacscore/gpg.el")
(load "~/.emacscore/ui.el")
(load "~/.emacscore/org/orgmode.el")
(load "~/.emacscore/jit-and-gc.el")
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
(load "~/.emacscore/dev/js.el")

;; Load miscellaneous things
(when (not (string-match-p "microsoft" (shell-command-to-string "uname -a")))
  (load "~/.emacscore/term.el")) ;; do not load vterm on WSL, does not work anyway
(load "~/.emacscore/snippets.el")
(load "~/.emacscore/build/make.el")
(load "~/.emacscore/markups.el")
(load "~/.emacscore/dotfiles.el")

;; do not use tabs for indentation at all
(setq-default indent-tabs-mode nil)

;; Hide *async-native-compile* buffer
(setq warning-minimum-level :error)

;; kill unneded buffers
(when (get-buffer "*straight-process*")
  (kill-buffer "*straight-process*"))
(when (get-buffer "*straight-byte-compilation*")
  (kill-buffer "*straight-byte-compilation*"))

;; finally loaded everything
(message "All done, %s%s" (user-login-name) ".")
