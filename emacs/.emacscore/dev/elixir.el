;;; elixir.el --- Elixir routines for GNU Emacs  -*- lexical-binding: t -*-

(require 'elixir-mode)

;; add lsp-mode for elixir-mode
(add-hook 'elixir-mode-hook 'lsp)

(setq lsp-elixir-local-server-command "/home/alex/dev/elixir-ls/release/language_server.sh")
(setq lsp-elixir-dialyzer-enabled t)
(setq lsp-elixir-fetch-deps t)
(setq lsp-elixir-mix-env "test")
