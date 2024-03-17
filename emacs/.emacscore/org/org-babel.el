;;; .emacs --- My org-mode configuration  -*- lexical-binding: t -*-

(defun org-eval-code-block ()
  "Evaluate code block inside org-babel code block and drop the buffers/windows
that appears during evaluation."
  (interactive)
  (org-babel-remove-result-one-or-many t)
  (org-open-at-point)
  (kill-buffer "*Org Babel Results*")
  (delete-window))

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
