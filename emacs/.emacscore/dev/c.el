(require 'cc-mode)

;;
;; Indentation for C/C++ code
;;
(setq c-basic-offset 8)
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (c++-mode . "linux")
			(cc-mode . "linux")
                        (other . "linux")))
(setq c-default-style "bsd")
(setq-default tab-width 8 indent-tabs-mode t)
(define-key c-mode-base-map (kbd "RET") 'newline-and-indent)

;;
;; company mode for autocomplete
;;
(add-to-list 'load-path "~/.emacs.d/company-20170715.1035")
(add-to-list 'load-path "~/.emacs.d/company-c-headers-20170531.1330")
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-backends (delete 'company-semantic company-backends))
(define-key c-mode-map  [(tab)] 'company-complete)
(define-key c++-mode-map  [(tab)] 'company-complete)
(add-to-list 'company-backends 'company-c-headers)

;;
;; Some helpers
;;
(defun open-includes-dir ()
  "Open /usr/include in tree."
  (interactive)
  (load-additional-dev-modes)
  (neotree-dir "/usr/include"))
(define-key c-mode-base-map (kbd "C-t") 'open-includes-dir)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For linux kernel
;;
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(add-hook 'c-mode-common-hook
          (lambda ()
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(add-hook 'c-mode-hook
          (lambda ()
           (let ((filename (buffer-file-name)))
              (when (and filename
                         (string-match (expand-file-name "~/dev/linux")
                                       filename))
                (setq indent-tabs-mode t)
                (setq show-trailing-whitespace t)
                (c-set-style "linux-tabs-only")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
