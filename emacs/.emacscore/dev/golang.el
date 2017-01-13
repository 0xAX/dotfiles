;;
;; Load go-mode
;;
(add-to-list 'load-path "~/.emacs.d/go-mode")
(require 'go-mode)

;;
;; Add some go fmt before save
;;
(add-hook 'before-save-hook 'gofmt-before-save)
(setq-default gofmt-command "goimports")
(add-hook 'go-mode-hook 'go-eldoc-setup)
(add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode)))
