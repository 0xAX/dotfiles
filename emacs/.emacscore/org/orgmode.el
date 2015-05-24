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

(custom-set-variables
 '(org-directory "~/dev/learning/todo"))

(setq org-todo-keywords
      '((sequence "TODO" "IN PROGRESS" "|" "DONE")))

(setq org-todo-keyword-faces
      '(("TODO". (:foreground "red" :weight bold))
        ("IN PROGRESS". (:foreground "orange" :weight bold))
        ("DONE". (:foreground "green" :weight bold)))
)

(defun todo()
  (interactive)
  (find-file "~/dev/learning/todo/to_learn_and_configure.org")
)
