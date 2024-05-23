;;; jit-and-gc.el --- Auxilary things to improve performance of GNU Emacs  -*- lexical-binding: t -*-

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold (* 32 1024 1024)))

(setq gcmh-high-cons-threshold (* 1024 1024 1024)
      gcmh-idle-delay-factor 20
      jit-lock-defer-time 0.05
      read-process-output-max (* 1024 1024)
      package-native-compile t)

;; if you don't use RTL ever, this could improve perf
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t)

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)
