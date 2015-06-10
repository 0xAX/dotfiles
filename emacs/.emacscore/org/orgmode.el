(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map [(control tab)] nil)))

(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map [(control a)] nil)))

(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map [(control k)] nil)))

(setq org-todo-keywords
      '((sequence "TODO" "IN PROGRESS" "STATE" "|" "DONE")))

(setq org-todo-keyword-faces
      '(("TODO". (:foreground "red" :weight bold))
        ("IN PROGRESS". (:foreground "orange" :weight bold))
        ("STATE". (:foreground "SeaGreen3" :weight bold))
        ("DONE". (:foreground "green" :weight bold)))
)

(custom-set-variables
 '(org-directory "~/dev/learning/todo"))

(defun todo()
  (interactive)
  (dired "~/dev/learning/todo/")
)

(defun important_todo ()
  (interactive)
  (find-file "~/dev/learning/todo/todo_important.org")
  )
