;;; elixir.el --- Elixir routines for GNU Emacs  -*- lexical-binding: t -*-

(require 'elixir-mode)

(add-hook 'elixir-mode-hook 'eglot-ensure)

(require 'eglot)
(add-to-list 'eglot-server-programs `(elixir-mode "/home/alex/dev/elixir-ls/release/language_server.sh"))

;; add lsp-mode for C
(add-hook 'elixir-mode-hook 'lsp)


(defvar lsp-elixir--config-options (make-hash-table))

(setq lsp-elixir-local-server-command "/home/alex/dev/elixir-ls/release/language_server.sh")
(setq lsp-elixir-dialyzer-enabled nil)

(add-hook 'lsp-after-initialize-hook
            (lambda ()
              (lsp--set-configuration `(:elixirLS, lsp-elixir--config-options))))
