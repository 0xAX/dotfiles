;;; snippets.el --- Snippets for GNU emacs -*- lexical-binding: t -*-

(require 'yasnippet)

;; Enable yasnippets for all prog-mode(s)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; Set path to the snippets fetched by straight
(setq straight-yas-dir
      (concat user-emacs-directory "/straight/build/yasnippet-snippets/snippets"))

;; Configure snippets directories
(setq yas-snippet-dirs
      '(straight-yas-dir "~/.emacscore/snippets"))

;; Enable yas for all
(yas-reload-all)
(yas-global-mode 1)
