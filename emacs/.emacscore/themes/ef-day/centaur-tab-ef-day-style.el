;;; centaur-tab-ef-day-style.el --- centaur-tab styling for ef-day theme  -*- lexical-binding: t -*-

;; ef-day palette:
;;   bg-main #fff5ea  bg-inactive #f7efe6  bg-active #c9c0b8  bg-hl-line #f9e2b2
;;   fg-main #584141  fg-dim #63728f  blue #375cc6  red #ba2d2f
(custom-set-faces
 `(centaur-tabs-default ((t (:background "#fff5ea" :foreground "#584141" :box nil))))
 `(centaur-tabs-selected ((t (:background "#f9e2b2" :foreground "#584141" :weight bold :box nil))))
 `(centaur-tabs-unselected ((t (:background "#f7efe6" :foreground "#63728f" :box nil))))
 `(centaur-tabs-selected-modified ((t (:background "#f9e2b2" :foreground "#ba2d2f" :weight bold :box nil))))
 `(centaur-tabs-unselected-modified ((t (:background "#f7efe6" :foreground "#ba2d2f" :box nil))))
 `(centaur-tabs-modified-marker-selected ((t (:inherit 'centaur-tabs-selected-modified :foreground "#ba2d2f" :box nil))))
 `(centaur-tabs-active-bar-face ((t (:background "#375cc6" :foreground "#375cc6"))))
 `(centaur-tabs-modified-marker-unselected ((t (:inherit 'centaur-tabs-unselected-modified :foreground "#ba2d2f" :box nil)))))
