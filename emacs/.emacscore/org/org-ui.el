;;; .emacs --- My org-mode configuration  -*- lexical-binding: t -*-

;; disable weird indentation of standard org-mode
(setq
 org-adapt-indentation nil
 org-src-preserve-indentation nil
 org-edit-src-content-indentation 0)

;; Fontify code in code blocks
(setq org-src-fontify-natively t)

;; Pretify special symbols
(org-toggle-pretty-entities)

;; set of TODO keywords highligted in org-mode
(setq org-todo-keywords
      '((sequence "TODO"
                  "IN-PROGRESS"
                  "WAITING"
                  "RESEARCH"
                  "NEED-TESTS"
                  "DONE")))

(custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 0.70))))
  '(org-level-2 ((t (:inherit outline-2 :height 0.8))))
  '(org-level-3 ((t (:inherit outline-3 :height 0.799))))
  '(org-level-4 ((t (:inherit outline-4 :height 0.777))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))

;; use org-bullets to pretify org-mode documents
(require 'org-bullets)
(add-hook 'org-mode-hook
          (lambda () (progn
                       (electric-indent-mode -1)
                       (org-bullets-mode 1))))
