;;; .emacs --- My org-mode configuration  -*- lexical-binding: t -*-

;; set of TODO keywords highligted in org-mode
(setq org-todo-keywords
      '((sequence "TODO"
                  "IN-PROGRESS"
                  "WAITING"
                  "RESEARCH"
                  "NEED-TESTS"
                  "DONE")))

;; Set default custom faces for org-mode
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :height 0.8))))
 '(org-level-2 ((t (:inherit outline-2 :height 0.8))))
 '(org-level-3 ((t (:inherit outline-3 :height 0.799))))
 '(org-level-4 ((t (:inherit outline-4 :height 0.777))))
 '(org-level-5 ((t (:inherit outline-5 :height 0.6)))))

;; Custom faces for the block lines based on the theme we are using
(cond
 ((equal current-theme 'material)
  (custom-set-faces
   '(org-block-begin-line ((t (:extend t :background "#FAFAFA" :foreground "#4e342e" :box nil))))
   '(org-block-end-line ((t (:extend t :background "#FAFAFA" :foreground "#4e342e" :box nil))))
   '(org-block ((t (:extend t :background "#FAFAFA" :foreground "#212121"))))))
 ((equal current-theme 'capptuccin-machiato)
  (custom-set-faces
   '(org-block-begin-line ((t (:extend t :background "#24273a" :foreground "#cad3f5" :box nil))))
   '(org-block-end-line ((t (:extend t :background "#24273a" :foreground "#cad3f5" :box nil))))
   '(org-block ((t (:extend t :background "#24273a" :foreground "#cad3f5"))))))
 (t
  '()))

;; use org-bullets to pretify org-mode documents
(require 'org-bullets)
(add-hook 'org-mode-hook
          (lambda () (progn
                       (electric-indent-mode -1)
                       (org-bullets-mode 1))))

;; Set foreground for latex fragments to white
(when (equal current-theme 'nord)
  (setq org-format-latex-options
      (plist-put org-format-latex-options :foreground "White")))
