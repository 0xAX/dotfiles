(load "~/.emacs.d/lisp/nasm.el")
(require 'nasm-mode)
(add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\|S\\)$" . nasm-mode))
