;;; .emacs --- Desktop and WM settings  -*- lexical-binding: t -*-

;; If we are using i3wm, load related configuration.
(if (executable-find "i3")
  (let*
      ((i3-socket (shell-command-to-string "i3 --get-socketpath"))
       (i3 (file-exists-p (replace-regexp-in-string "\n$" "" i3-socket))))
    (if i3
        (progn
          (load "~/.emacscore/desktop/i3.el")
          (setq *i3* "true"))
      (setq *i3* "false")))
  (setq *i3* "false"))

;; If we are using hyprland, load related configuration
(if (and
     (string= (string-trim (or (getenv "XDG_SESSION_TYPE") "")) "wayland")
     (getenv "HYPRLAND_INSTANCE_SIGNATURE"))
    (progn
      (load "~/.emacscore/desktop/hyprland.el")
      (setq *hyprland* "true"))
  (setq *hyprland* "false"))
