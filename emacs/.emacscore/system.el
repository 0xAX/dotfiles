;;; system.el --- auxiliary functions for system management -*- lexical-binding: t -*-

(defun sudo-shell-command (command)
  "Execute COMMAND with sudo privileges securely.
Uses a temporary file to pass the password to avoid exposure in process lists."
  (let ((temp-file (make-temp-file "emacs-sudo-"))
        (password (read-passwd "Password: ")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert password))
          (set-file-modes temp-file #o600)
          (shell-command (format "sudo -S %s < %s" command temp-file)))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))
