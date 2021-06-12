;;; .emacs --- My org-mode configuration  -*- lexical-binding: t -*-

;; set path to org directory
(setq org-directory "~/dev/todo")
;; enable org-mode for *.org files
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; unset some keybindings, to make it more accustomed
(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map [(control tab)] nil)))

(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map [(control a)] nil)))

(add-hook 'org-mode-hook
          '(lambda ()
             (define-key org-mode-map [(control k)] nil)))

;; set of TODO keywords highligted in org-mode
(setq org-todo-keywords
      '(
        (sequence "TODO" "IN-PROGRESS" "WAITING" "RESEARCH" "NEED-TESTS" "DONE")))

(defun todo()
  "Open TODO directory"
  (interactive)
  (dired org-directory))

(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 0.999))))
  '(org-level-2 ((t (:inherit outline-2 :height 0.8))))
  '(org-level-3 ((t (:inherit outline-3 :height 0.799))))
  '(org-level-4 ((t (:inherit outline-4 :height 0.777))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
)

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
;; (setq org-ellipsis " …")
;; (setq org-bullets-bullet-list '("•"))
