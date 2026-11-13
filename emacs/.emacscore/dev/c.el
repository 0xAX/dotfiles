;;; c.el --- C/C++ for GNU Emacs  -*- lexical-binding: t -*-

;; cc-mode is loaded on the first C/C++ file, not at startup
(with-eval-after-load 'cc-mode
  ;; disable wrong indentation in C++ namespaces
  (c-set-offset 'innamespace 0))

;; Indentation for C/C++ code
(setq c-basic-offset 4)

;; clangd's on-type formatting runs after RET and strips the indentation
;; `newline-and-indent' just inserted on the empty line, leaving point at
;; column 0.  Let cc-mode indent; turn it off before lsp starts (depth -10).
(defun my-c-disable-lsp-on-type-formatting ()
  (setq-local lsp-enable-on-type-formatting nil))
(add-hook 'c-mode-hook #'my-c-disable-lsp-on-type-formatting -10)
(add-hook 'c++-mode-hook #'my-c-disable-lsp-on-type-formatting -10)

;; No ligatures in C: `enable-ligatures' fills `prettify-symbols-alist' in
;; `prog-mode-hook', which runs before this one.  Emptying it leaves
;; `global-prettify-symbols-mode' with nothing to display as ≤ ≥ ≠ →.
(defun my-c-disable-ligatures ()
  (setq-local prettify-symbols-alist nil)
  (prettify-symbols-mode -1))
(add-hook 'c-mode-hook #'my-c-disable-ligatures)

(when (executable-find "clangd")
  (progn
    (add-hook 'c-mode-hook 'lsp)
    (add-hook 'c++-mode-hook 'lsp)))
