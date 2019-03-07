;;; .emacs --- My org-mode configuration  -*- lexical-binding: t -*-

;; path to org directory
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
        (sequence "TODO" "|" "DONE")
        (sequence "IN PROGRESS" "|" "DONE")))

(setq org-todo-keyword-faces
  '(("TODO" . (:foreground "orange red" :weight bold))
    ("IN PROGRESS" . "gold")))

(defun todo()
  "Open TODO directory"
  (interactive)
  (dired org-directory))

;; Customization
(custom-set-faces
 '(org-level-1 ((t (:inherit outline-1 :background "#263238" :foreground "CadetBlue1" :box nil :weight bold :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :background "#263238" :foreground "CadetBlue2" :box nil :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :background "#263238" :foreground "CadetBlue3" :height 0.9))))
 '(org-level-4 ((t (:inherit outline-4 :background "#263238" :foreground "#00bfff" :height 0.8))))
 '(org-level-5 ((t (:inherit outline-5 :background "#263238" :foreground "DarkOrange1"))))
 '(org-level-6 ((t (:inherit outline-6 :background "#263238" :foreground "DarkOrange2"))))
 '(org-level-7 ((t (:inherit outline-7 :background "#263238" :foreground "DarkOrange3"))))
 '(org-level-8 ((t (:inherit outline-8 :background "#263238" :foreground "gold2"))))
 '(org-level-1 ((t (:inherit outline-1 :background "#263238" :foreground "CadetBlue1" :box nil :weight bold :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :background "#263238" :foreground "CadetBlue2" :box nil :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :background "#263238" :foreground "CadetBlue3" :height 0.9))))
 '(org-level-4 ((t (:inherit outline-4 :background "#263238" :foreground "#00bfff" :height 0.8))))
 '(org-level-5 ((t (:inherit outline-5 :background "#263238" :foreground "DarkOrange1"))))
 '(org-level-6 ((t (:inherit outline-6 :background "#263238" :foreground "DarkOrange2"))))
 '(org-level-7 ((t (:inherit outline-7 :background "#263238" :foreground "DarkOrange3"))))
 '(org-level-8 ((t (:inherit outline-8 :background "#263238" :foreground "gold2"))))
 '(org-todo ((t (:background "#263238" :foreground "orange red" :weight bold)))))
