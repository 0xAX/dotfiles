;;; jit-and-gc.el --- Auxilary things to improve performance of GNU Emacs  -*- lexical-binding: t -*-

;; Do not collect garbage while the minibuffer is open, so completion
;; stays responsive.  Remember the threshold instead of resetting it to a
;; fixed value, because other files (e.g. dev/lsp-mode.el) raise it.
(defvar my-saved-gc-cons-threshold nil
  "Value of `gc-cons-threshold' before the minibuffer was opened.")

(defun my-minibuffer-setup-hook ()
  (unless my-saved-gc-cons-threshold
    (setq my-saved-gc-cons-threshold gc-cons-threshold))
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  ;; Restore only when leaving the outermost minibuffer.
  (when (and my-saved-gc-cons-threshold
             (<= (minibuffer-depth) 1))
    (setq gc-cons-threshold my-saved-gc-cons-threshold
          my-saved-gc-cons-threshold nil)))

(setq jit-lock-defer-time 0.05
      read-process-output-max (* 1024 1024)
      package-native-compile t)

;; if you don't use RTL ever, this could improve perf
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)
