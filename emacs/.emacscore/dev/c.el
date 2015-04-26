(require 'cc-mode)

(setq-default c-basic-offset 8 c-default-style "linux")
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)
(setq-default tab-width 8 indent-tabs-mode t)
