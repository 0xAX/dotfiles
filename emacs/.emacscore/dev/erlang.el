(setq load-path (cons "~/.emacs.d/erlang/"
      load-path))

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

(setq erlang-indent-level 4)

(custom-set-variables
 '(indent-tabs-mode nil)
 )

(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)
(require 'erlang-start)
