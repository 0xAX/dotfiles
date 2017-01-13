;; .emacs
;;
;;
;;        DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;                Version 2, December 2004
;;
;; Everyone is permitted to copy and distribute verbatim or modified
;; copies of this license document, and changing it is allowed as long
;; as the name is changed.
;;
;;         DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;; TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
;;
;;  0. You just DO WHAT THE FUCK YOU WANT TO.
;;
;;
;;
;;                                                            0xAX :)
;;

(defun current-file-name ()
  (interactive)
  "Prints file name of buffer to minibuffer"
  (message (buffer-file-name)))

(defun reload-file ()
  (interactive)
  "reload a file in the current buffer"
  (find-alternate-file (file-name)))

(defun insert-bash-she-bang ()
  (interactive)
  "Insert #!/bin/bash under cursor"
  (insert "#!/bin/bash"))

(defun emacs-config ()
  (interactive)
  "Open ~/.emacs in new buffer"
  (find-file "~/.emacs"))

(defun rename-file-and-buffer ()
  (interactive)
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
  (interactive)
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
  (interactive)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(defun load-additional-dev-modes ()
  (interactive)
  "Load additional development modes (web, and etc....)"
  (load "~/.emacscore/dev/sed.el")
  (load "~/.emacscore/dev/nasm.el")
  (load "~/.emacscore/dev/shell.el")
  (load "~/.emacscore/dev/rust.el")
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
  (load "~/.emacs.d/lisp/rfc.el")
  (load "~/.emacscore/markups.el")
  ;;
  ;; Enable rfc reader
  ;;
  (require 'irfc)
  ;;
  ;; Load mode-specific keybindings after all modes will be
  ;; loaded
  ;;
  (load "~/.emacscore/keybindings-hooks.el")
  ;;
  ;; add neotree
  ;;
  (add-to-list 'load-path "~/.emacs.d/emacs-neotree")
  (require 'neotree)
  ;;
  ;; load discover-my-major
  ;;
  (add-to-list 'load-path "~/.emacs.d/discover-my-major")
  (require 'discover-my-major))
