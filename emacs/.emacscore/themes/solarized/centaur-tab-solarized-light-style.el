;;; centaur-tab-solarized-light-style.el --- centaur-tab styling for solarized-light  -*- lexical-binding: t -*-

;; solarized-light palette:
;;   base3 #fdf6e3 base2 #eee8d5 base1 #93a1a1 base01 #586e75 base0 #839496
;;   blue #268bd2 orange #cb4b16 magenta #d33682
(custom-set-faces
 `(centaur-tabs-default ((t (:background "#eee8d5" :foreground "#586e75" :box nil))))
 `(centaur-tabs-selected ((t (:background "#fdf6e3" :foreground "#586e75" :weight bold :box nil))))
 `(centaur-tabs-unselected ((t (:background "#eee8d5" :foreground "#93a1a1" :box nil))))
 `(centaur-tabs-selected-modified ((t (:background "#fdf6e3" :foreground "#cb4b16" :weight bold :box nil))))
 `(centaur-tabs-unselected-modified ((t (:background "#eee8d5" :foreground "#d33682" :box nil))))
 `(centaur-tabs-modified-marker-selected ((t (:inherit 'centaur-tabs-selected-modified :foreground "#cb4b16" :box nil))))
 `(centaur-tabs-active-bar-face ((t (:background "#268bd2" :foreground "#268bd2"))))
 `(centaur-tabs-modified-marker-unselected ((t (:inherit 'centaur-tabs-unselected-modified :foreground "#d33682" :box nil)))))
