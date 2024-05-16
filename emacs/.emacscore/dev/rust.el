(require 'rust-mode)

;; Rust style-guide recommends to use spaces:
;; https://doc.rust-lang.org/1.0.0/style/style/whitespace.html
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;; format the code on save
(setq rustic-format-on-save t)

;; prettify some symbols
(add-hook 'rust-mode-hook
          (lambda () (prettify-symbols-mode)))

;; enable LSP for rust if we have rust-analyzer
(when (executable-find "rust-analyzer")
  (add-hook 'rust-mode-hook #'lsp))
