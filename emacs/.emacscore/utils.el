(defun file-name ()
  "Prints file name of buffer"
  (interactive)
  (message (buffer-file-name)))

(defun reload-file ()
  "reload a file in the current buffer"
  (interactive)
  (find-alternate-file (file-name)))

(defun insert-bash-she-bang ()
  "Insert #!/bin/bash under cursor"
  (interactive)
  (insert "#!/bin/bash"))

(defun emacs-config ()
  "Open ~/.emacs in new buffer"
  (interactive)
  (find-file (file-name)))

(defun reload-emacs-config ()
  "Reload ~/.emacs"
  (interactive)
  (load "~/.emacs"))

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

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(defun load-additional-dev-modes ()
  "Load additional development modes (web, and etc....)"
  (interactive)
  (load "~/.emacscore/dev/web.el")
  (load "~/.emacscore/dev/golang.el")
  (load "~/.emacscore/dev/sed.el")
  (load "~/.emacscore/dev/nasm.el")
  (load "~/.emacscore/dev/shell.el")
  (load "~/.emacscore/org/orgmode.el")
  ;;
  ;; Enable rainbow mode
  ;;
  (add-to-list 'load-path "~/.emacs.d/rainbow")
  (require 'rainbow-mode)
  (rainbow-mode 1)
  ;;
  ;; YAML mode
  ;;
  (add-to-list 'load-path "~/.emacs.d/yaml")
  (require 'yaml-mode)
  ;;
  ;; restructuredTest mode
  ;;
  (load "~/.emacs.d/lisp/rst.el")
  (require 'rst)
  ;;
  ;; some build/term and text extensions
  ;;
  (load "~/.emacscore/build/make.el")
  (load "~/.emacscore/term.el")
  (load "~/.emacs.d/lisp/rfc.el")
  (load "~/.emacscore/markups.el")
  (load "~/.emacscore/dev/shell.el")
  ;;
  ;; Enable rfc reader
  ;;
  (require 'irfc))
