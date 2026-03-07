;;; mocha.el --- GNU Emacs styling for catppuccin mocha theme  -*- lexical-binding: t -*-

;; Set path to catppuccin-mocha theme
(setq catppuccin-theme-path
      (concat user-emacs-directory "/straight/build/catppuccin-theme"))

;; Load catppuccin-mocha theme
(load (concat catppuccin-theme-path "/catppuccin-theme.el"))
(setq catppuccin-flavor 'mocha)
(enable-theme 'catppuccin)

;; Load custom catppuccin-mocha faces for tabbar if it is enabled
(when (equal tab-mode 'tabbar)
  (load "~/.emacscore/themes/catppuccin/tabbar-catppuccin-style.el"))
