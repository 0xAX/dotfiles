;; gruvbox.el --- GNU Emacs styling for gruvbox theme  -*- lexical-binding: t -*-

;; Set path to gruvbox theme
(setq gruvbox-theme-path
      (concat user-emacs-directory "/straight/build/gruvbox-theme"))

;; Load gruvbox dark theme
(load (concat gruvbox-theme-path "/gruvbox-theme.el"))
(load (concat gruvbox-theme-path "/gruvbox-dark-hard-theme.el"))
(enable-theme 'gruvbox-dark-hard)

;; Load custom gruvbox faces for tabbar if it is enabled
(when (equal tab-mode 'tabbar)
  (load "~/.emacscore/themes/gruvbox/tabbar-gruvbox-style.el"))

;; Load custom gruvbox faces for centaur-tabs if it is enabled
(when (equal tab-mode 'centaur-tab)
 (load "~/.emacscore/themes/gruvbox/centaur-tab-gruvbox-style.el"))

;; General gruvbox faces (applied regardless of tab mode)
(custom-set-faces
 ;; Remove line number border/background and set proper colors
 '(line-number ((t (:foreground "#7c6f64" :background "#1d2021" :inherit nil))))
 '(line-number-current-line ((t (:foreground "#fabd2f" :background "#1d2021" :weight bold))))
 ;; Fix fringe (the area where line numbers appear)
 '(fringe ((t (:background "#1d2021"))))
 ;; Improve mode-line colors
 '(mode-line ((t (:foreground "#ebdbb2" :background "#3c3836" :box nil))))
 '(mode-line-inactive ((t (:foreground "#a89984" :background "#282828" :box nil))))
 ;; Better region selection color
 '(region ((t (:background "#504945"))))
 ;; Better cursor color
 '(cursor ((t (:background "#fe8019")))))
