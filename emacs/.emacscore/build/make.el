;;; make.el --- make(1) routines for GNU Emacs  -*- lexical-binding: t -*-

(require 'make-mode)

;; special hook to fix cua-mode in makefiles
;;
;; NOTE: not really sure why but when I am openning Makefile/GNUMakefile
;; in my GNU Emacs - Cua mode is broken by some reasons. Re-enabling
;; helps to fix it.
(add-hook 'makefile-mode-hook
          (lambda () (progn
                       (cua-mode nil)
                       (cua-mode t))))

;; Change the color of extra spaces in makefiles
(cond
 ((equal current-theme 'material)
  (set-face-attribute 'makefile-space nil :background "#ECEFF1"))
 ((equal current-theme 'catppuccin-macchiato)
  (set-face-attribute 'makefile-space nil :background "#363a4f"))
 ((equal current-theme 'catppuccin-mocha)
  (set-face-attribute 'makefile-space nil :background "#313244"))
 ((equal current-theme 'gruvbox)
  (set-face-attribute 'makefile-space nil :background "#3c3836"))
 ((equal current-theme 'nord)
  (set-face-attribute 'makefile-space nil :background "#434c5e"))
 (t
  (set-face-attribute 'makefile-space nil :background "#eee8d5")))
