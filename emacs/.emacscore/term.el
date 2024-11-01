;;; term.el --- terminal emulator configuration in GNU Emacs  -*- lexical-binding: t -*-

(require 'vterm)

;; bash is a standard shell
(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "/bin/bash")
(setenv "SHELL" shell-file-name)

;; Improves terminal emulator (vterm/eat) throughput
(setq read-process-output-max (* 2 1024 1024)
      process-adaptive-read-buffering nil
      fast-but-imprecise-scrolling t
      redisplay-skip-fontification-on-input t
      inhibit-compacting-font-caches t
      idle-update-delay 1.0)

(defun open-terminal (terminal-name)
  "Open terminal with custom buffer name"
    (interactive "sTerminal name: ")
    (vterm (concat "term-" terminal-name)))

;; vterm custom keybindings
(define-key vterm-mode-map (kbd "C-c C-v") #'vterm-yank)

;; unset some common keybindings for vterm
(add-hook 'vterm-mode-hook
          (lambda ()
            ;; unbind keybinding we use to switch between emacs tabs
            (local-unset-key (kbd "<C-left>"))
            (local-unset-key (kbd "<C-right>"))
            ;; unbind keybinding to switch between i3 workspaces
            (local-unset-key (kbd "M-1"))
            (local-unset-key (kbd "M-2"))
            (local-unset-key (kbd "M-3"))
            (local-unset-key (kbd "M-4"))
            (local-unset-key (kbd "M-5"))
            (local-unset-key (kbd "M-6"))
            (local-unset-key (kbd "M-7"))
            (local-unset-key (kbd "M-8"))
            (local-unset-key (kbd "M-9"))))
