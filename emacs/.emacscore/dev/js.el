;;; js.el --- SLIME configuration -*- lexical-binding: t -*-

;; Highlight javascript files from the Mozilla Firefox source code
(add-to-list 'auto-mode-alist '("\\.sys\\.mjs\\'" . javascript-mode))

;; Set indentation for javascript
(setq js-indent-level 2)

(defun npm-and-typescript-ls-installed ()
  "Check that `typescript` and `typescript-language-server` are installed.

Looks for their executables in `exec-path' instead of running
`npm list -g', which takes about a second at every startup."
  (interactive)
  (and (executable-find "tsc")
       (executable-find "typescript-language-server")))

;; Enable LSP for javascript/typescript
(when (npm-and-typescript-ls-installed)
  (use-package lsp-mode
    :hook ((typescript-mode . lsp)
           (tsx-mode . lsp))
    :commands lsp
    :init
    (setq lsp-keymap-prefix "C-c l")))
