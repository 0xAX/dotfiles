;;; text-utils.el --- Text utilities for GNU Emacs  -*- lexical-binding: t -*-

(defun mark-curr-line ()
  "Set mark for current line"  
  (interactive)
  (beginning-of-line)
  (cua-set-mark)
  (end-of-line))

(defun replace-all (find replace)
  "Find & Replace all"
  (interactive
   (list
        (read-string "Find: ")
        (read-string "Replace with: ")))
  ;; Same case rules as `replace-string': an upper-case letter in FIND
  ;; makes the search case-sensitive.
  (let ((case-fold-search (and case-fold-search
                               (isearch-no-upper-case-p find nil)))
        (count 0))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward find nil t)
        (replace-match replace nil t)
        (setq count (1+ count))))
    (message "Replaced %d occurrence%s" count (if (= count 1) "" "s"))))

(defun kill-full-line()
  "Kill line from the beginnig of line"  
  (interactive)
  (beginning-of-line)
  (kill-line)
  (delete-backward-char 1))

(defun enable-ligatures ()
  (global-prettify-symbols-mode 1)
  (add-hook 'prog-mode-hook
          (lambda ()
            (push '("<=" . ?≤) prettify-symbols-alist)
            (push '(">=" . ?≥) prettify-symbols-alist)
            (push '("!=" . ?≠) prettify-symbols-alist)
            (push '("->" . ?→) prettify-symbols-alist))))

(defun revert-buffer-utf8 ()
  "Revert current buffer using UTF-8."
  (interactive)
  (revert-buffer-with-coding-system 'utf-8))
