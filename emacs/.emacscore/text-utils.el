;;; text-utils.el --- Text utilities for GNU Emacs  -*- lexical-binding: t -*-
;;
;; Author:  Alexander Kuleshov <kuleshovmail@gmail.com>
;; URL:     https://github.com/0xAX/med
;;
;; License: See LICENSE file.

(defun mark-curr-line ()
  "Set mark for current line"  
  (interactive)
  (beginning-of-line)
  (cua-set-mark)
  (end-of-line))

;; replace all
(defun replace-all (find replace)
  "Find & Replace all"
  (interactive
   (list
        (read-string "Find: ")
        (read-string "Replace with: ")))
  (setq cursor-position (point))
  (beginning-of-buffer)
  (replace-string find replace)
  (goto-char cursor-position)
  (message "done"))

;; kill full line
(defun kill-full-line()
  "Kill line from the beginnig of line"  
  (interactive)
  (beginning-of-line)
  (kill-line)
  (delete-backward-char 1))
