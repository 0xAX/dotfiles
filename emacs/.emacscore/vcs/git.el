;;
;; GIT&TOOLS configuration
;;
(add-to-list 'load-path "~/.emacs.d/git-commit-mode")
(add-to-list 'load-path "~/.emacs.d/git-rebase-mode")
(add-to-list 'load-path "~/.emacs.d/magit")

(autoload 'magit-status "magit" nil t)

(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list "~/.emacs.d/magit/")))
