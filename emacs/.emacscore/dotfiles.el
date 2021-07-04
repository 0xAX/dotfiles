;;; dotfiles.el --- auxiliary functions for dotfiles -*- lexical-binding: t -*-

(defconst dotfiles--script
  "~/dev/dotfiles/dotfiles"
  "Path to dotfiles script on my machine")

(defun update-dotfiles-repo ()
  "Update .dotfiles git repository"
  (interactive)
  (dotfiles--execute-cmd "--update-repo")
  (message "Done"))

(defun update-system-dotfiles ()
  "Update system dotfiles"
  (interactive)
  (dotfiles--execute-cmd "--update-system")
  (message "Done"))

(defun dotfiles--execute-cmd (cmd)
  (if (and (file-exists-p dotfiles--script)
           (file-executable-p dotfiles--script))
      (let ((dotfiles-proc (start-process-shell-command
                            "dotfiles"
                            "*dotfiles*"
                            (concat dotfiles--script " " cmd))))
        (while (eq 'run (process-status dotfiles-proc))
          :wait)
        (when (zerop (process-exit-status dotfiles-proc))
          (kill-buffer "*dotfiles*")))
    (display-warning :error "dotfiles script not found")))
