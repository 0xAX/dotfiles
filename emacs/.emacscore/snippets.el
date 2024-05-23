(require 'yasnippet)

(add-hook 'prog-mode-hook #'yas-minor-mode)

(setq yas-dir
      (concat user-emacs-directory "/elpa/yasnippet-snippets/snippets"))

;; Configure snippets directories
(setq yas-snippet-dirs
      '(yas-dir "~/.emacscore/snippets"))

;; Enable yas for all
(yas-reload-all)
(yas-global-mode 1)
