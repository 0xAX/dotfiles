;;; company.el --- C/C++ for GNU Emacs  -*- lexical-binding: t -*-

;; company mode for autocomplete
(add-to-list 'load-path "~/.emacs.d/company")
(add-to-list 'load-path "~/.emacs.d/company-c-headers")

(require 'cc-mode)
(require 'company)

;; enable company mode
(add-hook 'after-init-hook 'global-company-mode)

;; update company backends
(add-to-list
 'company-backends
 'company-c-headers)
 
;; List of paths to search C/C++ headers
(setq company-c-headers-path-system
      '("/usr/include/c++/8" "/usr/include" "/usr/local/include"))

;; company mode keybindings for C/C++
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)

;; (setq company-minimum-prefix-length 1)
;; (setq company-idle-delay 0.1)
(push '(company-rtags company-keywords) company-backends)
