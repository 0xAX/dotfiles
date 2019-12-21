(add-to-list 'load-path "~/.emacs.d/slime")
(require 'slime-autoloads)

(setq inferior-lisp-program
      (shell-command-to-string "command -v sbcl"))

(setq slime-lisp-implementations
      '((sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))
(setq slime-default-lisp 'sbcl)

(eval-after-load "slime"
  '(progn
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol
           slime-fuzzy-completion-in-place t
           slime-enable-evaluate-in-emacs t
           slime-autodoc-use-multiline-p t)
     (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)
     (define-key slime-mode-map (kbd "C-c i") 'slime-inspect)
     (define-key slime-mode-map (kbd "C-c C-s") 'slime-selector)))

(slime-setup '(slime-fancy slime-quicklisp slime-asdf))
