(require 'yasnippet)

(add-hook 'prog-mode-hook #'yas-minor-mode)

(setq straight-yas-dir
      (concat user-emacs-directory "/straight/build/yasnippet-snippets/snippets"))

;; Configure snippets directories
(setq yas-snippet-dirs
      '(straight-yas-dir "~/.emacscore/snippets"))

;; Enable yas for all
(yas-reload-all)
(yas-global-mode 1)
