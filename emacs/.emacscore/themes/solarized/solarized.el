;; solarized.el --- GNU Emacs styling for solarized theme  -*- lexical-binding: t -*-

;; Set path to solarized theme
(setq solarized-theme-path
      (concat user-emacs-directory "/straight/build/solarized-emacs"))

;; Load solarized-light theme
(load (concat solarized-theme-path "/solarized-palettes.el"))
(load (concat solarized-theme-path "/solarized-faces.el"))
(load (concat solarized-theme-path "/solarized.el"))
(load (concat solarized-theme-path "/solarized-theme.el"))
(load (concat solarized-theme-path "/solarized-light-theme.el"))
(enable-theme 'solarized-light)

;; Load custom solarized-light faces for tabbar if it is enabled
(when (equal tab-mode 'tabbar)
  (load "~/.emacscore/themes/solarized/tabbar-solarized-light-style.el"))
