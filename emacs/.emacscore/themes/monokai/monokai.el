;; monokai.el --- GNU Emacs styling for monokai theme  -*- lexical-binding: t -*-

;; Set path to monokai theme
(setq monokai-theme-path
      (concat user-emacs-directory "/straight/build/monokai-theme"))

;; Load monokai theme
(load (concat monokai-theme-path "/monokai-theme.el"))
(enable-theme 'monokai)

;; Load custom monokai faces for tabbar if it is enabled
(when (equal tab-mode 'tabbar)
  (load "~/.emacscore/themes/monokai/tabbar-monokai-style.el"))

;; Load custom monokai faces for centaur-tabs if it is enabled
(when (equal tab-mode 'centaur-tab)
 (load "~/.emacscore/themes/monokai/centaur-tab-monokai-style.el"))

;; General monokai theme faces (applied regardless of tab mode)
(custom-set-faces
 ;; Remove line number border/background and set proper colors
 '(line-number ((t (:foreground "#8F908A" :background "#272822" :inherit nil))))
 '(line-number-current-line ((t (:foreground "#FD971F" :background "#272822" :weight bold))))
 ;; Fix fringe (the area where line numbers appear)
 '(fringe ((t (:background "#272822"))))
 ;; Improve mode-line colors
 '(mode-line ((t (:foreground "#F8F8F2" :background "#49483E" :box nil))))
 '(mode-line-inactive ((t (:foreground "#75715E" :background "#3E3D31" :box nil))))
 ;; Better region selection color
 '(region ((t (:background "#49483E"))))
 ;; Better cursor color
 '(cursor ((t (:background "#FD971F"))))
 ;; The monokai theme renders a unique ido match (`ido-only-match', e.g. when a
 ;; find-file path narrows down to a single file) with a yellow background,
 ;; which looks like a yellow region/selection. Drop the background and use a
 ;; plain green foreground instead.
 '(ido-only-match ((t (:foreground "#A6E22E" :background nil :weight normal))))
 ;; The monokai theme underlines the matched prefix (`company-tooltip-common')
 ;; in the auto-complete popup, which shows up as underscored text on the
 ;; non-selected candidates. Remove the underline.
 '(company-tooltip-common ((t (:foreground "#66D9EF" :underline nil))))
 '(company-preview-common ((t (:foreground "#66D9EF" :underline nil))))
 ;; The monokai theme styles `show-paren-match' with `:inverse-video t', which
 ;; turns the matched expression (we use `show-paren-style' = expression) into a
 ;; glaring green block. Use a subtle dark highlight instead, no inverse-video.
 '(show-paren-match ((t (:foreground "#FD971F" :background "#6b6450" :inverse-video nil :weight bold))))
 '(show-paren-match-expression ((t (:foreground unspecified :background "#514c3b" :inverse-video nil)))))
