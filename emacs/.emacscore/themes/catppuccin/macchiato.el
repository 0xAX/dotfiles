;;; macchiato.el --- GNU Emacs styling for catppuccin macchiato theme  -*- lexical-binding: t -*-

;; Set path to catppuccin-macchiato theme
(setq catppuccin-theme-path
      (concat user-emacs-directory "/straight/build/catppuccin-theme"))

;; Load catppuccin-macchiato theme
(setq catppuccin-flavor 'macchiato)
(load (concat catppuccin-theme-path "/catppuccin-theme.el"))
(enable-theme 'catppuccin))

;; Load custom catppuccin-macchiato faces for tabbar if it is enabled
(when (equal tab-mode 'tabbar)
  (load "~/.emacscore/themes/catppuccin/tabbar-catppuccin-style.el"))
