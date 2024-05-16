(require 'rust-mode)

;; Rust style-guide recommends to use spaces:
;; https://doc.rust-lang.org/1.0.0/style/style/whitespace.html
(add-hook 'rust-mode-hook
          (lambda () (setq indent-tabs-mode nil)))

;; format the code on save
(setq rustic-format-on-save t)

;; set compilation command
(setq rustic-compile-command "cargo build")

;; prettify some symbols
(add-hook 'rustic-mode-hook
          (lambda ()
            (prettify-symbols-mode)))

;; rust-mode keybindings
(define-key rustic-mode-map (kbd "C-c b") 'rustic-cargo-bench)
(define-key rustic-mode-map (kbd "C-c c") 'rustic-compile)
(define-key rustic-mode-map (kbd "C-c t") 'rustic-cargo-test-run)

;; enable LSP for rust if we have rust-analyzer
(when (executable-find "rust-analyzer")
  (add-hook 'rust-mode-hook #'lsp))
