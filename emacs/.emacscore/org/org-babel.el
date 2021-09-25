;;; .emacs --- My org-mode configuration  -*- lexical-binding: t -*-

(defun org-execute-block ()
  "Execute code block inside BEGIN_SRC/END_SRC tags"
  (interactive)
  (cond
   ((string= (car (org-babel-get-src-block-info)) "latex")
    (org-babel-goto-src-block-head)
    (next-line)
    (let ((curr-buff (buffer-name))
          (begin-point (point))
          (end-point (progn
                       (search-forward "END_SRC")
                       (beginning-of-line)
                       (previous-line)
                       (end-of-line)
                       (point))))
      (set-mark begin-point)
      (goto-char end-point)
      (latex-math-preview-expression)
      (when (get-buffer "*latex-math-preview-expression*")
        (progn
          (switch-to-buffer "*latex-math-preview-expression*")
          (mark-whole-buffer)
          (cua-copy-region 1)
          (deactivate-mark)
          (kill-buffer "*latex-math-preview-expression*")
          (delete-window)
          (switch-to-buffer curr-buff)
          (goto-char end-point)
          (beginning-of-line)
          (next-line)
          (next-line)
          (insert "\n#+RESULTS:\n:RESULTS:")
          (cua-paste 1)
          (insert "\n:END:")))))
   (t (progn
        (shell-command-to-string "i3-msg mode default")
        (org-babel-execute-src-block)))))

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
              '(lambda () (org-execute-code-block)))
    (add-hook 'org-babel-after-execute-hook
              '(lambda () (shell-command-to-string "i3-msg mode passthrough")))))

;; remove confirmation for code execution,
;; I hope I know what I am doing
(setq org-confirm-babel-evaluate nil)
