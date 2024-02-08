;;; .emacs --- My org-mode configuration  -*- lexical-binding: t -*-

;; set path to org directory
(setq org-directory "~/dev/todo")
;; enable org-mode for *.org files
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Enable auto-search in org-mode so any non-standard keypress in C-x C-j
;; mode will lead to search according to pressed keys
(setq org-goto-auto-isearch t)

;; First of all unset some keybindings
(add-hook 'org-mode-hook
          #'(lambda ()
              (define-key org-mode-map [(control tab)] nil)
              (define-key org-mode-map [(control k)] nil)
              (define-key org-mode-map [(control a)] nil)
              ;; used for switching between left/right windows
              (define-key org-mode-map [(shift left)] nil)
              (define-key org-mode-map [(shift right)] nil)
              ;; used for switching between up/down windows
              (define-key org-mode-map [(shift down)] nil)
              (define-key org-mode-map [(shift up)] nil)
              ;; execute source code inside org-mode documents
              (define-key org-mode-map (kbd "C-x e") 'org-execute-block)
              (define-key org-mode-map (kbd "C-x C-e") 'org-execute-block)))

;; Enable support for org-tables everywhere
(add-hook 'message-mode-hook 'turn-on-orgtbl)
;; Disable linum mode for org-mode as it looks ugly there
(add-hook 'org-mode-hook #'(lambda () (linum-mode -1)))

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

;; remove results from org-mode buffers before exit
;; (add-hook 'kill-buffer-hook 'org-remove-all-results-blocks)

;; load additional org-mode helpers
(load "~/.emacscore/org/org-api.el")
(load "~/.emacscore/org/org-babel.el")
(load "~/.emacscore/org/org-ui.el")
