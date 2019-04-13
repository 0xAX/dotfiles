(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(yas-reload-all)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(setq yas-snippet-dirs
      '("~/.emacs.d/yasnippet-snippets/snippets"))
