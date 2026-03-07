;;; material.el --- GNU Emacs styling for material theme  -*- lexical-binding: t -*-

;; Set path to material theme
(setq material-theme-path
      (concat user-emacs-directory "/straight/build/material-theme"))

;; Load material light theme
(load (concat material-theme-path "/material-light-theme.el"))
(enable-theme 'material-light)

;; Load custom material faces for tabbar if it is enabled
(when (equal tab-mode 'tabbar)
  (load "~/.emacscore/themes/material/tabbar-material-style.el"))

;; Load custom material faces for centaur-tabs if it is enabled
(when (equal tab-mode 'centaur-tab)
 (load "~/.emacscore/themes/material/centaur-tab-material-style.el"))

;; General Material Light theme faces
(custom-set-faces
 ;; Line numbers
 '(line-number ((t (:foreground "#B0BEC5" :background "#FAFAFA" :inherit nil))))
 '(line-number-current-line ((t (:foreground "#2196F3" :background "#FAFAFA" :weight bold))))
 ;; Fringe
 '(fringe ((t (:background "#FAFAFA"))))
 ;; Mode-line
 '(mode-line ((t (:foreground "#212121" :background "#ECEFF1" :box (:line-width 1 :color "#CFD8DC")))))
 '(mode-line-inactive ((t (:foreground "#757575" :background "#F5F5F5" :box (:line-width 1 :color "#E0E0E0")))))
 ;; Region selection
 '(region ((t (:background "#BBDEFB"))))
 ;; Cursor color
 '(cursor ((t (:background "#2196F3"))))
 ;; Matching parentheses color
 '(show-paren-match ((t (:background "#BBDEFB"))))
 '(show-paren-match-expression ((t (:background "#BBDEFB")))))
