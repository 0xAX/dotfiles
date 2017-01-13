;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modes dependend key bindings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; dired
;;
(define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file)
(define-key dired-mode-map (kbd "C-<up>") (lambda () (interactive) (find-alternate-file "..")))

;; markdown
;;
(add-hook 'markdown-mode-hook (lambda () (local-set-key (kbd "C-p") #'markdown-preview)))
(add-hook 'markdown-mode-hook (lambda () (local-set-key (kbd "C-c l") #'markdown-insert-link)))
(add-hook 'markdown-mode-hook (lambda () (local-set-key (kbd "C-c p") #'markdown-insert-image)))
(add-hook 'markdown-mode-hook (lambda () (local-set-key (kbd "C-c b") #'markdown-insert-bold)))
(add-hook 'markdown-mode-hook (lambda () (local-set-key (kbd "C-c i") #'markdown-insert-italic)))
(add-hook 'markdown-mode-hook (lambda () (local-set-key (kbd "C-c q") #'markdown-insert-blockquote)))

;;
;; Emacs lisp
;;
(add-hook 'emacs-lisp-mode-hook (lambda () (local-set-key (kbd "\C-e") #'eval-buffer)))

;;
;; Rust mode
;;
(add-hook 'rust-mode-hook (lambda () (local-set-key (kbd "TAB") #'company-indent-or-complete-common)))
