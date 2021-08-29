;;; system.el --- auxiliary functions for system management -*- lexical-binding: t -*-

(defun sudo-shell-command (command)
  (shell-command (concat "echo " (read-passwd "Password: ") " | sudo -S " command)))
