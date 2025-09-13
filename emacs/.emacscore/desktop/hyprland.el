;;; hyprland.el --- Do not conflict with hyprland WM keybindings  -*- lexical-binding: t -*-

;;
;; exit from hyprland passthrough mode on exit
;;
(add-hook 'kill-emacs-hook
          (lambda ()
            (shell-command-to-string "hyprctl dispatch submap reset")))

;;
;; switch to the given workspace.
;;
(defmacro hyprland-switch-workspace (workspace)
  "Generate a function to switch to the hyprland workspace."
  `(defun ,(intern (concat "go-to-workspace-" workspace)) ()
     (interactive)
     (shell-command-to-string "hyprctl dispatch submap reset")
     (shell-command-to-string (concat "hyprctl dispatch workspace " , workspace))))

(hyprland-switch-workspace "1")
(hyprland-switch-workspace "2")
(hyprland-switch-workspace "3")
(hyprland-switch-workspace "4")
(hyprland-switch-workspace "5")
(hyprland-switch-workspace "6")
(hyprland-switch-workspace "7")
(hyprland-switch-workspace "8")
(hyprland-switch-workspace "9")

(defun switch-to-next-hyprland-workspace ()
  "switch to the next hyprland workspace"
  (interactive)
  (shell-command-to-string "hyprctl dispatch submap reset")
  (shell-command-to-string "hyprctl dispatch workspace +1"))

;;
;; go to the default namespace if we're in emacs window
;;
(hyprland-switch-workspace "2")
