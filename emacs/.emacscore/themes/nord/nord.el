;; nord.el --- GNU Emacs styling for nord theme  -*- lexical-binding: t -*-

;; Load nord theme
(load "~/.emacscore/lisp/nord-theme.el")
(enable-theme 'nord)

;; Customize colors of  centaur-tabs for nord
(custom-set-faces
 `(centaur-tabs-default ((t (:background ,"#434c5e" :foreground ,"#eceff4" :box nil))))
 `(centaur-tabs-selected ((t (:background ,"#434c5e" :foreground ,"#d8dee9" :box nil))))
 `(centaur-tabs-unselected ((t (:background ,"#2e3440" :foreground ,"#d8dee9" :box nil))))
 `(centaur-tabs-selected-modified ((t (:background ,"#434c5e" :foreground ,"#eceff4" :box nil))))
 `(centaur-tabs-unselected-modified ((t (:background ,"#434c5e" :foreground ,"#eceff4" :box nil))))
 `(centaur-tabs-modified-marker-selected ((t (:inherit 'centaur-tabs-selected-modified :foreground ,"#d8dee9" :box nil))))
 `(centaur-tabs-active-bar-face ((t (:background ,"#88c0d0" :foreground ,"#88c0d0"))))
 `(centaur-tabs-modified-marker-unselected ((t (:inherit 'centaur-tabs-unselected-modified :background "#434c5e" :foreground ,"#d8dee9" :box nil)))))

;; Change foreground for mode-line if theme is set to nord
(custom-set-faces
 `(mode-line ((t (:foreground ,"#D8DEE9" :background ,"#4C566A"))))
 `(font-lock-comment-face ((,t (:foreground ,"#72809a"))))
 `(font-lock-comment-delimiter-face ((,t (:foreground ,"#72809a"))))
 `(line-number ((,t (:foreground ,"#72809a")))))
