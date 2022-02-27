;;; c.el --- C/C++ for GNU Emacs  -*- lexical-binding: t -*-
(require 'cc-mode)

;; disable wrong indentation in C++ namespaces
(c-set-offset 'innamespace 0)

;; Indentation for C/C++ code
(setq c-basic-offset 4)

;; add lsp-mode for C
;;(add-hook 'c-mode-hook 'lsp)
