;; Initialization of magit

(add-to-list 'load-path "~/.emacs.d/dash.el")
(add-to-list 'load-path "~/.emacs.d/magit-deps/transient/lisp")
(add-to-list 'load-path "~/.emacs.d/magit-deps/with-editor")

(add-to-list 'load-path "~/.emacs.d/magit/lisp")
(require 'magit)

(with-eval-after-load 'info
  (info-initialize)
  (add-to-list 'Info-directory-list
               "~/.emacs.d/site-lisp/magit/Documentation/"))
