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

;; If we are using sway, load related configuration. SWAYSOCK is the socket
;; sway itself exports, so it is set only inside a running sway session - no
;; need to look for a binary the way the i3 branch above has to.
(if (getenv "SWAYSOCK")
    (progn
      (load "~/.emacscore/desktop/sway.el")
      (setq *sway* "true"))
  (setq *sway* "false"))
