;;; python.el --- Python configuration -*- lexical-binding: t -*-

;; Enable Python LSP
(add-hook 'python-mode-hook 'lsp-deferred)
