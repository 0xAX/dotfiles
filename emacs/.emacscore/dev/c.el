;;; c.el --- C/C++ for GNU Emacs  -*- lexical-binding: t -*-
(require 'cc-mode)

(c-set-offset 'innamespace 0)

;; Indentation for C/C++ code
(setq c-basic-offset 4)
(setq c-default-style '((java-mode . "java")
                        (awk-mode  . "awk")
			(cc-mode   . "linux")
                        (other     . "linux")))

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
                         (string-match (expand-file-name "~/dev/dev/linux")
                                       filename))
                (setq indent-tabs-mode t)
                (setq show-trailing-whitespace t)
                (c-set-style "linux-tabs-only")))))
