;;; utils.el --- Auxiliary functions for GNU Emacs  -*- lexical-binding: t -*-

(defun reload-file ()
  "reload a file in the current buffer"  
  (interactive)
  (find-alternate-file (buffer-file-name)))

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

(defun new-file ()
  "Open new file with Untitled-<N> name where `N' is
a number incresed by 1 from the greatest already opened
Untitled-<N> file."
  (interactive)
  (let ((highest-untitled (get--next-untitled)))
    (find-file
     (cond ((eq highest-untitled nil) "Untitled-1")
           (t (concat "Untitled-" (number-to-string
                                   (+ 1 (string-to-number
                                         (car (last (split-string highest-untitled "-"))))))))))))

(defun get--next-untitled ()
  (car
   (last
    (cl-sort (seq-filter
              (lambda (buf) (= 0 (or (string-match "^Untitled\-[0-9]+$" buf) -1)))
              (mapcar (function buffer-name) (buffer-list)))
             'string-version-lessp))))
