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

;; Shortcuts for storing links, viewing the agenda, and starting a capture
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(define-key global-map "\C-cc" 'org-capture)

;; Remap the change priority keys to use the UP or DOWN key
(define-key org-mode-map (kbd "C-c <up>") 'org-priority-up)
(define-key org-mode-map (kbd "C-c <down>") 'org-priority-down)
