;;; .emacs --- My org-mode configuration  -*- lexical-binding: t -*-

;; Evaluate org-mode code block with removing results before
(define-key org-mode-map (kbd "C-c C-o") nil)
(define-key org-mode-map (kbd "C-c C-o") 'org-eval-code-block)

;; used for switching between tabs
(define-key org-mode-map [(control tab)] nil)
;; used for switching between left/right windows
(define-key org-mode-map [(control k)] nil)
(define-key org-mode-map [(control a)] nil)
;; used for switching between up/down windows
(define-key org-mode-map [(shift down)] nil)
(define-key org-mode-map [(shift up)] nil)
;; show the help for special symbols
(define-key org-mode-map (kbd "C-x C-m") 'org-entities-help)
