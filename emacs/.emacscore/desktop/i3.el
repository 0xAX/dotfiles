;;; i3.el --- Do not conflict with i3 WM keybindings  -*- lexical-binding: t -*-

;;
;; exit from i3 passthrough mode on exit
;;
(add-hook 'kill-emacs-hook
          (lambda ()
            (shell-command-to-string "i3-msg mode default")))

;;
;; switch to the given workspace.
;;
(defmacro i3-switch-workspace (workspace)
  "Generate a function to switch to the i3 workspace."
  `(defun ,(intern (concat "go-to-workspace-" workspace)) ()
     (interactive)
     (shell-command-to-string "i3-msg mode default")
     (shell-command-to-string (concat "i3-msg workspace number " , workspace))))

(i3-switch-workspace "1")
(i3-switch-workspace "2")
(i3-switch-workspace "3")
(i3-switch-workspace "4")
(i3-switch-workspace "5")
(i3-switch-workspace "6")
(i3-switch-workspace "7")
(i3-switch-workspace "8")
(i3-switch-workspace "9")

(defun switch-to-next-i3-workspace ()
  "switch to the next i3 workspace"
  (interactive)
  (shell-command-to-string "i3-msg mode default")
  (shell-command-to-string "i3-msg workspace next"))

;;
;; go to the default namespace if we're in emacs window
;;
(i3-switch-workspace "2")
