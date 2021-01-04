;;; common-lisp.el --- SLIME configuration -*- lexical-binding: t -*-

;; load SLIME
(add-to-list 'load-path "~/.emacs.d/slime")
(require 'slime-autoloads)

;; setup path to lisp compiler
(setq inferior-lisp-program "/usr/bin/sbcl")

;; setup lisp implementations
(setq slime-lisp-implementations
      '((sbcl ("/usr/bin/sbcl") :coding-system utf-8-unix)))

;; set default lisp
(setq slime-default-lisp 'sbcl)

;; add for SLIME contribs
(setq slime-contribs '(slime-scratch
		       slime-asdf
		       slime-editing-commands))

;; try to start swank faster
(when (file-exists-p "~/sbcl.core-for-slime")
  (setq slime-lisp-implementations
	'((sbcl ("sbcl" "--core" "~/sbcl.core-for-slime")))))

(defun slime-keybindings ()
  (local-set-key (kbd "C-h s")   'slime-describe-symbol)
  (local-set-key (kbd "C-h f")   'slime-describe-function)
  (local-set-key (kbd "M-l")     'slime-load-system))

(add-hook 'slime-mode-hook          #'slime-keybindings)
(add-hook 'slime-repl-mode-hook     #'slime-keybindings)
