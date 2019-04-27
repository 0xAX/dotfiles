(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(add-hook 'prog-mode-hook #'yas-minor-mode)

(setq yas-snippet-dirs
      '("~/.emacs.d/yasnippet-snippets/snippets" "~/.emacs.d/snippets"))
