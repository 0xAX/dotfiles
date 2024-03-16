;;; .emacs --- My org-mode configuration  -*- lexical-binding: t -*-

(require 'org)

;; set path to org directory
(when (file-directory-p "~/dev/todo")
  (setq org-directory "~/dev/todo"))
(when (file-directory-p "~/disk/dev/todo")
  (setq org-directory "~/disk/dev/todo"))

;; Add ability to add closing notes to the done items
(setq org-log-done 'note)

;; enable org-mode for *.org files
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Enable auto-search in org-mode so any non-standard keypress in C-x C-j
;; mode will lead to search according to pressed keys
(setq org-goto-auto-isearch t)

;;
;; First of all change and unset some keybindings
;;
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

;; Enable support for org-tables everywhere
(add-hook 'message-mode-hook 'turn-on-orgtbl)

(defun org-remove-all-results-blocks ()
  "Go through the org buffer and remove all RESULTS"
  (interactive)
  (when (string= (symbol-name major-mode) "org-mode")
    (progn
      (goto-char (point-min))
      (while (re-search-forward "RESULTS" nil t)
        (org-babel-remove-result-one-or-many t))
      (setq buffer-save-without-query t)
      (save-buffer)
      (setq buffer-save-without-query nil))))

;; load additional org-mode helpers
(load "~/.emacscore/org/org-api.el")
(load "~/.emacscore/org/org-babel.el")
(load "~/.emacscore/org/org-ui.el")
