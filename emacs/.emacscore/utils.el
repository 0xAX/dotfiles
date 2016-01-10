;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  (find-file "~/.emacs"))

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
;; Open file with sudo
;;
(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Help helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Print helper keybindings
;;
(setq emacs-tutorial "C-h t help-with-tutorial ----> emacs tutorial\n")
(setq emacs-keybindings "C-h k describe-key       ----> keybindings help\n")
(setq emacs-function    "C-h f describer-function ----> emacs function\n")
(setq emacs-cur-buf-keys "C-h m describe-mode      ----> qkeybindings for current buffer\n")
(setq emacs-manual "C-h r help-with-tutorial ----> emacs manual\n")

(defun helpers ()
  "Print emacs help combinations to minibuffer"
  (interactive)
  (message (concat emacs-tutorial emacs-keybindings emacs-function emacs-cur-buf-keys emacs-manual))
)

;;
;; Load additional development modes
;;
(defun load-additional-dev-modes ()
  "Load additional development modes (web, and etc....)"
  (interactive)
  (load "~/.emacscore/dev/web.el")
  (load "~/.emacscore/dev/golang.el")
  (load "~/.emacscore/dev/sed.el")
  (load "~/.emacscore/org/orgmode.el")
  ;;
  ;; Enable rainbow mode
  ;;
  (add-to-list 'load-path "~/.emacs.d/rainbow")
  (require 'rainbow-mode)
  (rainbow-mode 1))
