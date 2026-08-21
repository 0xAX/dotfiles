;;; sway.el --- Do not conflict with sway WM keybindings  -*- lexical-binding: t -*-

;;
;; exit from sway passthrough mode on exit
;;
;; ~/.config/sway/emacs.pl puts sway into `passthrough' mode before switching
;; to the emacs workspace, so that Alt stays with emacs.  Nothing else leaves
;; that mode except $mod+Escape, which would strand the keyboard the moment
;; emacs is gone.
;;
(add-hook 'kill-emacs-hook
          (lambda ()
            (shell-command-to-string "swaymsg mode default")))

;;
;; switch to the given workspace.
;;
(defmacro sway-switch-workspace (workspace)
  "Generate a function to switch to the sway WORKSPACE."
  `(defun ,(intern (concat "go-to-workspace-" workspace)) ()
     (interactive)
     (shell-command-to-string "swaymsg mode default")
     (shell-command-to-string (concat "swaymsg workspace number " , workspace))))

(sway-switch-workspace "1")
(sway-switch-workspace "2")
(sway-switch-workspace "3")
(sway-switch-workspace "4")
(sway-switch-workspace "5")
(sway-switch-workspace "6")
(sway-switch-workspace "7")
(sway-switch-workspace "8")
(sway-switch-workspace "9")

(defun switch-to-next-sway-workspace ()
  "switch to the next sway workspace"
  (interactive)
  (shell-command-to-string "swaymsg mode default")
  (shell-command-to-string "swaymsg workspace next"))
