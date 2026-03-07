;;; dired.el --- My configuration for dired  -*- lexical-binding: t -*-

;;
;; Dired configuration
;;
(require 'dired-x)
(require 'dired)
(setq dired-listing-switches "-lapq --author --file-type --group-directories-first
                              -h --human-readable --hide='*.o' --hide='*.d' ")

;; open file in a dired
(put 'dired-find-alternate-file 'disabled nil)

;; Reuse the same buffer when navigating directories
;; This prevents creating tons of tabs/buffers
(add-hook 'dired-mode-hook
          (lambda ()
            (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
            (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))))

;; Use nerd fonts in dired if possible
(when
    (or
     (file-exists-p "/usr/share/fonts/fira-code/FiraCodeNerdFont-Regular.ttf")
     (file-exists-p "~/.local/share/fonts/FiraCodeNerdFont-Regular.ttf"))
  (require 'nerd-icons)
  (require 'nerd-icons-dired)
  (add-hook 'dired-mode-hook #'nerd-icons-dired-mode))
