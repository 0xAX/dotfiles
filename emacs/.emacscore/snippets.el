;;; snippets.el --- Snippets for GNU emacs -*- lexical-binding: t -*-

;; Enable yasnippets for all prog-mode(s)
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; Set path to the snippets fetched by straight
(setq straight-yas-dir
      (concat user-emacs-directory "/straight/build/yasnippet-snippets/snippets"))

;; Configure snippets directories
(setq yas-snippet-dirs
      '(straight-yas-dir "~/.emacscore/snippets"))

;; Enable yas for all.  Loading yasnippet and all snippet collections takes
;; over a second, so do it right after startup instead of during it.
(run-with-idle-timer 0.5 nil
                     (lambda ()
                       (require 'yasnippet)
                       (yas-reload-all)
                       (yas-global-mode 1)))
