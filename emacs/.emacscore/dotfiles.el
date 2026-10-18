;;; dotfiles.el --- auxiliary functions for dotfiles -*- lexical-binding: t -*-

(require 'comint)

(defconst dotfiles--script
  "~/dev/dotfiles/dotfiles"
  "Path to dotfiles script on my machine")

(defun update-dotfiles-repo ()
  "Update .dotfiles git repository"
  (interactive)
  (dotfiles--execute-cmd "--update-repo"))

(defun update-system-dotfiles ()
  "Update system dotfiles"
  (interactive)
  (dotfiles--execute-cmd "--update-system"))

(defun dotfiles--filter (proc output)
  "Insert OUTPUT of PROC and answer password prompts (e.g. from sudo)."
  (with-current-buffer (process-buffer proc)
    (goto-char (point-max))
    (insert output))
  (when (string-match-p comint-password-prompt-regexp output)
    (process-send-string
     proc (concat (read-passwd (string-trim output)) "\n"))))

(defun dotfiles--sentinel (proc _event)
  (when (memq (process-status proc) '(exit signal))
    (if (zerop (process-exit-status proc))
        (progn (kill-buffer (process-buffer proc))
               (message "dotfiles: done"))
      (display-buffer (process-buffer proc))
      (message "dotfiles: failed, see *dotfiles*"))))

(defun dotfiles--execute-cmd (cmd)
  (if (and (file-exists-p dotfiles--script)
           (file-executable-p dotfiles--script))
      (make-process :name "dotfiles"
                    :buffer (get-buffer-create "*dotfiles*")
                    :command (list (expand-file-name dotfiles--script) cmd)
                    :connection-type 'pty
                    :filter #'dotfiles--filter
                    :sentinel #'dotfiles--sentinel)
    (display-warning :error "dotfiles script not found")))
