;;; company.el --- C/C++ for GNU Emacs  -*- lexical-binding: t -*-

(require 'cc-mode)
(require 'company)

;; enable company mode
(add-hook 'after-init-hook 'global-company-mode)

;; disable delay before showing extensions
(setq company-idle-delay 0)
; Show suggestions after entering one character
(setq company-minimum-prefix-length 1)
;; wrap around menu
(setq company-selection-wrap-around t)

;; update company backends
(add-to-list
 'company-backends
 'company-c-headers
 'company-yasnippet)
 
;; List of paths to search C/C++ headers
(setq company-c-headers-path-system
      '("/usr/include/c++/8" "/usr/include" "/usr/local/include"))

;; company mode keybindings for C/C++
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)
