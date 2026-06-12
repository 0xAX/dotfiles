;;; centaur-tab-catppuccin-style.el --- centaur-tab styling for catppuccin themes  -*- lexical-binding: t -*-

;; catppuccin ships two flavors here (mocha, macchiato); pick the matching
;; palette so the centaur-tabs bar looks the same size/shape as the other
;; themes (the global tab font + tab-line height are pinned in tabs.el).
(cond
 ((equal catppuccin-flavor 'mocha)
  ;; mocha: base #1e1e2e mantle #181825 surface0 #313244 text #cdd6f4
  ;;        overlay #9399b2 blue #89b4fa peach #fab387 red #f38ba8
  (custom-set-faces
   `(centaur-tabs-default ((t (:background "#1e1e2e" :foreground "#cdd6f4" :box nil))))
   `(centaur-tabs-selected ((t (:background "#313244" :foreground "#cdd6f4" :weight bold :box nil))))
   `(centaur-tabs-unselected ((t (:background "#181825" :foreground "#9399b2" :box nil))))
   `(centaur-tabs-selected-modified ((t (:background "#313244" :foreground "#fab387" :weight bold :box nil))))
   `(centaur-tabs-unselected-modified ((t (:background "#181825" :foreground "#f38ba8" :box nil))))
   `(centaur-tabs-modified-marker-selected ((t (:inherit 'centaur-tabs-selected-modified :foreground "#fab387" :box nil))))
   `(centaur-tabs-active-bar-face ((t (:background "#89b4fa" :foreground "#89b4fa"))))
   `(centaur-tabs-modified-marker-unselected ((t (:inherit 'centaur-tabs-unselected-modified :foreground "#f38ba8" :box nil))))))
 ((equal catppuccin-flavor 'macchiato)
  ;; macchiato: base #24273a mantle #1e2030 surface0 #363a4f text #cad3f5
  ;;            overlay #8087a2 blue #8aadf4 peach #f5a97f red #ed8796
  (custom-set-faces
   `(centaur-tabs-default ((t (:background "#24273a" :foreground "#cad3f5" :box nil))))
   `(centaur-tabs-selected ((t (:background "#363a4f" :foreground "#cad3f5" :weight bold :box nil))))
   `(centaur-tabs-unselected ((t (:background "#1e2030" :foreground "#8087a2" :box nil))))
   `(centaur-tabs-selected-modified ((t (:background "#363a4f" :foreground "#f5a97f" :weight bold :box nil))))
   `(centaur-tabs-unselected-modified ((t (:background "#1e2030" :foreground "#ed8796" :box nil))))
   `(centaur-tabs-modified-marker-selected ((t (:inherit 'centaur-tabs-selected-modified :foreground "#f5a97f" :box nil))))
   `(centaur-tabs-active-bar-face ((t (:background "#8aadf4" :foreground "#8aadf4"))))
   `(centaur-tabs-modified-marker-unselected ((t (:inherit 'centaur-tabs-unselected-modified :foreground "#ed8796" :box nil)))))))
