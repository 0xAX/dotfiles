;;; c.el --- C/C++ for GNU Emacs  -*- lexical-binding: t -*-
(require 'cc-mode)

;; disable wrong indentation in C++ namespaces
(c-set-offset 'innamespace 0)

;; Indentation for C/C++ code
(setq c-basic-offset 4)

(when (executable-find "clangd")
  (progn
    (add-hook 'c-mode-hook 'lsp)
    (add-hook 'c++-mode-hook 'lsp)))
