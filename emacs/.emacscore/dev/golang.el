;;
;; Load go-mode
;;
(add-to-list 'load-path "~/.emacs.d/go-mode")
(require 'go-mode)

;;
;; Add some go fmt before save
;;
(add-hook 'before-save-hook 'gofmt-before-save)
