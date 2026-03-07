;;; centaur-tab-gruvbox-style.el --- centaur-tab styling for gruvbox theme  -*- lexical-binding: t -*-

(custom-set-faces
 `(centaur-tabs-default ((t (:background "#1d2021" :foreground "#ebdbb2" :box nil))))
 `(centaur-tabs-selected ((t (:background "#504945" :foreground "#fbf1c7" :weight bold :box nil))))
 `(centaur-tabs-unselected ((t (:background "#32302f" :foreground "#928374" :box nil))))
 `(centaur-tabs-selected-modified ((t (:background "#504945" :foreground "#fe8019" :weight bold :box nil))))
 `(centaur-tabs-unselected-modified ((t (:background "#32302f" :foreground "#d65d0e" :box nil))))
 `(centaur-tabs-modified-marker-selected ((t (:inherit 'centaur-tabs-selected-modified :foreground "#fe8019" :box nil))))
 `(centaur-tabs-active-bar-face ((t (:background "#d65d0e" :foreground "#d65d0e"))))
 `(centaur-tabs-modified-marker-unselected ((t (:inherit 'centaur-tabs-unselected-modified :foreground "#d65d0e" :box nil)))))
