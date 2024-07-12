;;; elixir.el --- Elixir routines for GNU Emacs  -*- lexical-binding: t -*-

(require 'elixir-mode)

(setq lsp-elixir-script-path
      "/home/alex/dev/elixir-ls/release/language_server.sh")

;; (when (file-exists-p lsp-elixir-script-path)
;;   (add-hook 'elixir-mode-hook 'lsp)
;;   (setq lsp-elixir-local-server-command
;;         lsp-elixir-script-path)
;;   (setq lsp-elixir-dialyzer-enabled t)
;;   (setq lsp-elixir-fetch-deps t)
;;   (setq lsp-elixir-mix-env "test"))
