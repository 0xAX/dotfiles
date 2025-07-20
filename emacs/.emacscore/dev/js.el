;;; js.el --- SLIME configuration -*- lexical-binding: t -*-

;; Highlight javascript files from the Mozilla Firefox source code
(add-to-list 'auto-mode-alist '("\\.sys\\.mjs\\'" . javascript-mode))

;; Set indentation for javascript
(setq js-indent-level 2)

(defun npm-and-typescript-ls-installed ()
  "Check that `npm` is installed and that `typescript` and `typescript-language-server`
are globally installed."
  (interactive)
  (let ((npm-exists (executable-find "npm")))
    (if (not npm-exists)
        (progn
          (warn "npm is not installed or not in PATH.")
          nil)
      (let ((npm-list-output (shell-command-to-string "npm list -g --depth=0 --parseable")))
        (and
         (seq-filter (lambda (package) (string= (file-name-base package) "typescript"))
                     (string-split test-ts-var "\n"))
         (seq-filter (lambda (package) (string= (file-name-base package) "typescript-language-server"))
                     (string-split test-ts-var "\n")))))))

;; Enable LSP for javascript/typescript
(when (npm-and-typescript-ls-installed)
  (use-package lsp-mode
    :hook ((typescript-mode . lsp)
           (tsx-mode . lsp))
    :commands lsp
    :init
    (setq lsp-keymap-prefix "C-c l")))
