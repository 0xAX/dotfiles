;;; sed-mode.el --- A major mode for editing sed source code

;; Copyright (C) 2013-2014 Eric Schulte

;; Author: Eric Schulte <schulte.eric@gmail.com>
;; Version: 0.1
;; Keywords: languages, sed
;; Description: A major mode for editing sed source code

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; A major mode for editing sed source code.

;;; Code:
(defcustom sed-mode-hook '()
  "Normal hook run when entering `sed-mode'."
  :type 'hook
  :group 'sed)

(defvar sed-mode-map (make-sparse-keymap)
  "Key map for `sed-mode'.")

(defvar sed-range-regexp
  (concat "\\(" (mapconcat #'identity
                           (list "[0-9]\+"
                                 "[0-9]\+,[0-9]\+"
                                 "[0-9]\+~[0-9]\+"
                                 "[0-9]\+,/[^\n/]*/[IM]?"
                                 "[0-9]\+,\\$"
                                 "/[^\n/]*/[IM]?"
                                 "/[^\n/]*/[IM]?,[0-9]\+"
                                 "/[^\n/]*/[IM]?,/[^\n/]*/[IM]?"
                                 "/[^\n/]*/[IM]?,\\$")
                           "\\|")
          "\\)[ \t]*")
  "Regexp to match range expressions in `sed-mode'.")

(defconst sed-commands
  (list
   ;; common
   ?q ?d ?p ?n ?s
   ;; others
   ?y ?a ?i ?c ?= ?l ?r ?w ?D ?N ?P ?h ?g ?G ?x ?H
   ;; GNU sed
   ?e ?F ?L ?Q ?R ?T ?v ?W ?z)
  "Commands used in `sed-mode'.
See (info \"(sed)sed Programs\").")

(defconst sed-prog-commands (list ?: ?b ?t)
  "Programming commands used in `sed-mode'.
See (info \"(sed)Programming Commands\").")

(defconst sed-escapes
  '("a"
    "f"
    "n"
    "r"
    "t"
    "v"
    "c."
    "d[0-9][0-9][0-9]"
    "o[0-9][0-9][0-9]"
    "x[0-9][0-9]"
    "w"
    "W"
    "b"
    "B"
    "`"
    "'")
  "Escape sequences in `sed-mode'.
See (info \"(sed)Escapes\").")

(defconst sed-font-lock-keywords
  (eval-when-compile
    `(;; definitions
      (,(concat "\\(^\\|;\\|{\\)\\(" sed-range-regexp "\\)?"
                "\\(" (regexp-opt-charset sed-commands) "\\)")
       (4 font-lock-builtin-face))
      ;; control structures
      (,(concat "\\(^\\|;\\|{\\)"
                "\\(" sed-range-regexp "\\)?"
                "\\(" (regexp-opt-charset sed-prog-commands) "\\)"
                "[ \t]*"
                "\\([^ \t\n\r]\+\\)")
       (4 font-lock-builtin-face)
       (5 font-lock-function-name-face))
      ;; back references
      ("\\\\[0-9]" . font-lock-variable-name-face)
      ;; special escape sequences
      (,(concat "\\(^\\|[^\\\\]\\)\\(\\\\\\("
                (mapconcat #'identity sed-escapes "\\|")
                "\\)\\)")
       (2 font-lock-string-face))
      ;; TODO: errors (malformed commands)
      ;; ("" 1 font-lock-warning-face)
      ))
  "Default expressions to highlight in `sed-mode'.")

(defvar sed-mode-syntax-table
  (let ((table (make-syntax-table)))
    (dolist (pair '((?\\ . "\\")        ; escape
                    ;; (?\/ . "\"")        ; string quotes
                    (?\" . "w")         ; normal word character
                    (?\# . "<")         ; open comment
                    (?\n . ">")))       ; close comment
      (modify-syntax-entry (car pair) (cdr pair) table))
    table)
  "Syntax table to use in `sed-mode'.")

;;;###autoload
(define-derived-mode sed-mode prog-mode "sed"
  "Major mode for editing sed code."
  :syntax-table sed-mode-syntax-table
  ;; set a variety of local variables
  ((lambda (local-vars)
     (dolist (pair local-vars)
       (set (make-local-variable (car pair)) (cdr pair))))
   `((adaptive-fill-mode . nil)
     (comment-start . "# ")
     (comment-start-skip . "^\\s-*#\\(?: \\|$\\)")
     (comment-add . 0)
     (comment-use-global-state . nil)
     (comment-multi-line . nil)
     (mode-name . "sed")
     (font-lock-defaults . (sed-font-lock-keywords)))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.sed\\'" . sed-mode))

(provide 'sed-mode)
