;;; git-init.el --- Minimal Emacs config for git operations -*- lexical-binding: t -*-
;;
;; Usage: emacs -nw -q -l ~/.emacscore/git-init.el FILE
;;
;; Configure git to use this:
;;   git config --global core.editor "emacs -nw -q -l ~/.emacscore/git-init.el"
;;

;; Basic settings
(setq inhibit-splash-screen t
      make-backup-files nil
      auto-save-list-file-name nil
      auto-save-default nil
      create-lockfiles nil)

;; UTF-8
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

;; Minimal UI for terminal
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Line numbers
(when (>= emacs-major-version 26)
  (global-display-line-numbers-mode 1))

;; Highlight current line
(global-hl-line-mode 1)

;; y/n instead of yes/no
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show matching parens
(show-paren-mode 1)

;; Disable bell
(setq ring-bell-function 'ignore)

;; CUA mode for familiar copy/paste
(cua-mode t)
(setq cua-auto-tabify-rectangles nil)
(transient-mark-mode 1)

;; Column numbers in modeline
(column-number-mode t)

;; Reuse existing straight.el packages
(setq user-emacs-directory "~/.emacs.d")

;; Transient settings (shared with main config)
(setq transient-history-file "~/.emacscore/transient/transient-history.el")
(setq transient-values-file "~/.emacscore/transient/transient-values.el")

;; Bootstrap straight.el (reuse existing installation)
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 7))
  (when (file-exists-p bootstrap-file)
    (load bootstrap-file nil 'nomessage)))

;; Load magit
(straight-use-package 'magit)
;; Disable auto-revert to avoid the void variable error
(setq magit-auto-revert-mode nil)
(require 'magit)

;; Magit settings (from your main config)
(setq transient-default-level 7)
(setq magit-log-section-commit-count 50)

;; with-editor for proper C-c C-c / C-c C-k in commit buffers
(straight-use-package 'with-editor)
(require 'with-editor)

(message "Minimal git config loaded.")
