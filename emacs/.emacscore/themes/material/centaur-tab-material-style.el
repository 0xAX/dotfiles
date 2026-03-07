;;; centaur-tab-material-style.el --- Centaur-tabs styles for Material Light theme  -*- lexical-binding: t -*-

;; Material Light theme colors for centaur-tabs
(custom-set-faces
 ;; Main tab faces
 `(centaur-tabs-default ((t (:background "#FAFAFA" :foreground "#212121" :box nil))))
 `(centaur-tabs-selected ((t (:background "#FFFFFF" :foreground "#212121" :weight bold :box nil))))
 `(centaur-tabs-unselected ((t (:background "#ECEFF1" :foreground "#757575" :box nil))))
 `(centaur-tabs-selected-modified ((t (:background "#FFFFFF" :foreground "#2196F3" :weight bold :box nil))))
 `(centaur-tabs-unselected-modified ((t (:background "#ECEFF1" :foreground "#03A9F4" :box nil))))
 `(centaur-tabs-modified-marker-selected ((t (:inherit 'centaur-tabs-selected-modified :foreground "#2196F3" :box nil))))
 `(centaur-tabs-active-bar-face ((t (:background "#2196F3" :foreground "#2196F3"))))
 `(centaur-tabs-modified-marker-unselected ((t (:inherit 'centaur-tabs-unselected-modified :foreground "#03A9F4" :box nil)))))
