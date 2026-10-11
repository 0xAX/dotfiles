;;; rust.el --- Rust for GNU Emacs  -*- lexical-binding: t -*-

;; rust-mode and rustic are loaded when the first .rs file is opened
;; (rustic also pulls in org for its babel support, so loading it at
;; startup would load org too).  Make sure .rs files open in rustic-mode,
;; which is what loading rustic used to arrange.
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))

;; Add rust binaries to exec-path
(add-to-list 'exec-path "/home/alex/.cargo/bin")
(setenv "PATH" (concat "/home/alex/.cargo/bin:" (getenv "PATH")))

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
(with-eval-after-load 'rustic
  (define-key rustic-mode-map (kbd "C-c b") 'rustic-cargo-bench)
  (define-key rustic-mode-map (kbd "C-c c") 'rustic-compile)
  (define-key rustic-mode-map (kbd "C-c t") 'rustic-cargo-test-run))

;; enable LSP for rust if we have rust-analyzer
;; cover both major modes: rust-mode buffers and rustic-mode buffers
(when (executable-find "rust-analyzer")
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'rustic-mode-hook #'lsp))

;; flycheck's bare `rust' checker runs `rustc' directly without an --edition
;; flag, so it defaults to edition 2015 and falsely rejects edition-2024
;; features (e.g. let-chains). Disable it; `rust-cargo'/lsp read Cargo.toml.
(with-eval-after-load 'flycheck
  (add-to-list 'flycheck-disabled-checkers 'rust))
