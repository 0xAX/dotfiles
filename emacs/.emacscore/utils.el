;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Prints file name of current buffer
;;
(defun file-name ()
  "Prints file name of buffer"
  (interactive)
  (message (buffer-file-name)))

;;
;; Open .emacs in new buffer
;;
(defun emacs-config ()
  "Open ~/.emacs in new buffer"
  (interactive)
  (find-file "~/.emacs")
)

;;
;; Open rss config file
;;
(defun open-rss-config ()
  "Open ~/.emacs/rss/rss.el in new buffer"
  (interactive)
  (find-file "~/.emacscore/rss/rss.el"))

;;
;; Rename file and buffer helper
;;
(defun rename-file-and-buffer ()
   "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

;;
;; Delete current buffer and file
;;
(defun delete-this-buffer-and-file ()
  "Remove file connected to current buffer and kill buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))

;;
;; Print helper keybindings
;;
(setq emacs-tutorial "C-h t help-with-tutorial ----> emacs tutorial\n")
(setq emacs-keybindings "C-h k describe-key       ----> keybindings help\n")
(setq emacs-function    "C-h f describer-function ----> emacs function\n")
(setq emacs-cur-buf-keys "C-h m describe-mode      ---->  keybindings for current buffer\n")
(setq emacs-manual "C-h r help-with-tutorial ----> emacs manual\n")

(defun helpers ()
  "Print emacs help combinations to minibuffer"
  (interactive)
  (message (concat emacs-tutorial emacs-keybindings emacs-function emacs-cur-buf-keys emacs-manual))
)

;;
;; read string from file
;;
(defun get-string-from-file (filePath)
  "Return filePath's file content."
  (with-temp-buffer
    (insert-file-contents filePath)
    (buffer-string)))

;;
;; write string to file
;;
(defun write-string-to-file (string file)
  (interactive "sEnter the string: \nFFile to save to: ")
  (with-temp-buffer
    (insert string)
    (when (file-writable-p file)
      (write-region (point-min)
                    (point-max)
                    file))))

;;
;; Open new terminal with name
;;
(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "bash")
(setenv "SHELL" shell-file-name)
(setenv "ESHELL" shell-file-name)

(defun open-terminal (terminal-name)
  "Open terminal with custom buffer name"
  (interactive
   (list
	(read-string "Buffer name: ")))
    (ansi-term (getenv "SHELL") (concat "term: " terminal-name)))

;;
;; mark current line
;;
(defun mark-curr-line ()
  "Set mark for current line"
  (interactive)
  (beginning-of-line)
  (cua-set-mark)
  (end-of-line))

;;
;; replace all
;;
(defun replace-all (find replace)
  "Find&Replace all"
  (interactive
   (list
	(read-string "Find: ")
	(read-string "Replace with: ")))
  (setq cursor-position (point))
  (beginning-of-buffer)
  (replace-string find replace)
  (goto-char cursor-position)
  (message "done"))

;;
;; kill full line
;;
(defun kill-full-line()
  "Kill line from the beginnig of line"
  (interactive)
  (beginning-of-line)
  (kill-line)
  (delete-backward-char 1))
