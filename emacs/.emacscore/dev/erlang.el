;;; erlang.el --- Erlang routines for GNU Emacs  -*- lexical-binding: t -*-

(setq exec-path (cons "/usr/local/lib/erlang/bin" exec-path))

(add-to-list 'auto-mode-alist '("\\.erl?$" . erlang-mode))
(add-to-list 'auto-mode-alist '("\\.hrl?$" . erlang-mode))

(setq erlang-root-dir "/usr/lib/erlang")
(add-to-list 'exec-path "/usr/lib/erlang/bin")
(setq erlang-man-root-dir "/usr/lib/erlang/man")

(defun my-erlang-mode-hook ()
  "Imenu for erlang."
  (setq inferior-erlang-machine-options '("-sname" "emacs"))
  (imenu-add-to-menubar "imenu")
  (local-set-key [return] 'newline-and-indent)
  (set-frame-height (selected-frame) 20))

(defun erl-shell (flags)
   "Start an erlang shell with FLAGS."
   (interactive (list (read-string "Flags: ")))
   (set 'inferior-erlang-machine-options (split-string flags))
   (set-frame-height  5)
   (erlang-shell))

(setq erlang-mode-hook
    (function (lambda ()
		(setq indent-tabs-mode t)
		(setq tab-width 8)
		(setq c-indent-level 4))))

(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
(require 'erlang-start nil t)
