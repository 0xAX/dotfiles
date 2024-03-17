;;; .emacs --- My org-mode configuration  -*- lexical-binding: t -*-

(defun org-eval-code-block ()
  "Evaluate code block inside org-babel code block and drop the buffers/windows
that appears during evaluation."
  (interactive)
  (org-babel-remove-result-one-or-many t)
  (org-open-at-point)
  (if (not (get-buffer "*Org-Babel Error Output*"))
      (progn
        (kill-buffer "*Org Babel Results*")
        (delete-window))
    (switch-to-buffer "*Org-Babel Error Output*" 'norecord t)))

;; List of langauges supported by babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (C . t)
   (emacs-lisp . t)
   (lisp . t)
   (plantuml . t)
   (latex . t)
   (octave . t)
   (perl . t)
   (scheme . t)
   (sql . t)))

;; Do not execute code on C-c C-c by default as it will be re-binded
(setq org-babel-no-eval-on-ctrl-c-ctrl-c t)

;; If we are in i3 environment add special hooks for code block
;; execution to avoid dances with i3 modes
(when (string= *i3* "true")
  (progn
    (add-hook 'org-babel-execute-src-block-hook
              #'(lambda () (org-execute-code-block)))
    (add-hook 'org-babel-after-execute-hook
              #'(lambda () (shell-command-to-string "i3-msg mode passthrough")))))

;; remove confirmation for code execution,
;; I hope I know what I am doing
(setq org-confirm-babel-evaluate nil)

;; Default flags passed to each C code block
(defvar org-babel-default-header-args:C
  '((:flags . "-Wall -Wextra -Werror -WWstrict-prototypes  -Wcast-qual -Wconversion -Wpedantic -std=c17")))
