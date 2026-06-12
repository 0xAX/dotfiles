;;; centaur-tab-monokai-style.el --- centaur-tab styling for monokai theme  -*- lexical-binding: t -*-

(custom-set-faces
 `(centaur-tabs-default ((t (:background "#272822" :foreground "#F8F8F2" :box nil))))
 `(centaur-tabs-selected ((t (:background "#49483E" :foreground "#F8F8F0" :weight bold :box nil))))
 `(centaur-tabs-unselected ((t (:background "#3E3D31" :foreground "#75715E" :box nil))))
 `(centaur-tabs-selected-modified ((t (:background "#49483E" :foreground "#FD971F" :weight bold :box nil))))
 `(centaur-tabs-unselected-modified ((t (:background "#3E3D31" :foreground "#F92672" :box nil))))
 `(centaur-tabs-modified-marker-selected ((t (:inherit 'centaur-tabs-selected-modified :foreground "#FD971F" :box nil))))
 `(centaur-tabs-active-bar-face ((t (:background "#FD971F" :foreground "#FD971F"))))
 `(centaur-tabs-modified-marker-unselected ((t (:inherit 'centaur-tabs-unselected-modified :foreground "#F92672" :box nil)))))
