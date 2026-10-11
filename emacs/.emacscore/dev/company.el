;;; company.el --- C/C++ for GNU Emacs  -*- lexical-binding: t -*-

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
(add-to-list 'company-backends 'company-c-headers)

;; Offer snippets together with the candidates of every backend.  A
;; standalone `company-yasnippet' would shadow all backends after it, as
;; company only asks the first backend that claims the prefix.
(defun company-backends-with-yasnippet (backends)
  "Return BACKENDS with `company-yasnippet' merged into each of them."
  (mapcar (lambda (backend)
            (if (and (listp backend) (memq 'company-yasnippet backend))
                backend
              (append (if (consp backend) backend (list backend))
                      '(:with company-yasnippet))))
          backends))

(setq company-backends (company-backends-with-yasnippet company-backends))

;; lsp-mode pushes a bare `company-capf' in front of the buffer-local
;; backends, so merge snippets into it again.
(add-hook 'lsp-completion-mode-hook
          (lambda ()
            (setq-local company-backends
                        (company-backends-with-yasnippet company-backends))))

;; List of paths to search C/C++ headers
(setq company-c-headers-path-system
      '("/usr/include/c++/8" "/usr/include" "/usr/local/include"))

;; company mode keybindings for C/C++, set when cc-mode is loaded
(with-eval-after-load 'cc-mode
  (define-key c-mode-map  [(tab)] 'company-complete)
  (define-key c++-mode-map  [(tab)] 'company-complete))
