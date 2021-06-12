;;; common-lisp.el --- SLIME configuration -*- lexical-binding: t -*-

;; load SLIME
(add-to-list 'load-path "~/.emacs.d/slime")
(require 'slime-autoloads)

;; setup path to lisp compiler
(when (file-exists-p "/usr/bin/sbcl")
  (setq inferior-lisp-program "/usr/bin/sbcl"))
(when (file-exists-p "/usr/local/bin/sbcl")
  (setq inferior-lisp-program "/usr/local/bin/sbcl"))

;; set default lisp
(setq slime-default-lisp 'sbcl)

;; add for SLIME contribs
(setq slime-contribs '(slime-scratch
		       slime-asdf
		       slime-editing-commands
		       slime-autodoc))

;; try to start swank faster
(when (file-exists-p "~/sbcl.core-for-slime")
  (setq slime-lisp-implementations
	'((sbcl ("sbcl" "--core" "~/sbcl.core-for-slime")))))

(defun switch-to-slime-repl ()
  "Switch to SLIME repl or open a new one if it is not exists"
  (interactive)
  (dolist (buffer (buffer-list))
    (when (equal (format "%s" buffer) "*slime-repl sbcl*")
      (progn
	(switch-to-buffer "*slime-repl sbcl*")
	(return buffer))))
  (slime-repl))

(defun slime-keybindings ()
  ;; slime help
  (local-set-key (kbd "C-h s")   'slime-describe-symbol)
  (local-set-key (kbd "C-h f")   'slime-describe-function)
  (local-set-key (kbd "C-h a")   'slime-apropos)
  (local-set-key (kbd "C-h A")   'slime-apropos-all)
  (local-set-key (kbd "C-h P")   'slime-apropos-package)
  (local-set-key (kbd "C-h h")   'slime-hyperspec-lookup)
  (local-set-key (kbd "M-l")     'slime-load-system)
  ;; lisp evaluation
  (local-set-key (kbd "C-x f") 'slime-eval-defun)
  (local-set-key (kbd "C-x i") 'slime-interactive-eval)
  (local-set-key (kbd "C-x r") 'switch-to-slime-repl)
  ;; internals
  (local-set-key (kbd "C-x d") 'slime-disassemble-symbol)
  ;; repl commands
  (local-set-key (kbd "C-x l") 'slime-repl-clear-buffer))

(add-hook 'slime-mode-hook          #'slime-keybindings)
(add-hook 'slime-repl-mode-hook     #'slime-keybindings)
