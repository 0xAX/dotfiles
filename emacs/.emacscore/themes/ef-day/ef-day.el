;; ef-day.el --- GNU Emacs styling for ef-day theme  -*- lexical-binding: t -*-

;; ef-day is part of the `ef-themes' collection (installed via straight in
;; .emacs). Unlike the hand-written themes, the ef-themes package ships its own
;; well-tuned faces for line-numbers, fringe, mode-line, region, ido, company,
;; etc. -- so here we only load it and add the centaur-tabs / tabbar styling
;; that ef-themes does not cover.

;; Load and enable the ef-day theme
(require 'ef-themes)
(load-theme 'ef-day t)

;; Load custom ef-day faces for tabbar if it is enabled
(when (equal tab-mode 'tabbar)
  (load "~/.emacscore/themes/ef-day/tabbar-ef-day-style.el"))

;; Load custom ef-day faces for centaur-tabs if it is enabled
(when (equal tab-mode 'centaur-tab)
 (load "~/.emacscore/themes/ef-day/centaur-tab-ef-day-style.el"))

;; Load custom ef-day faces for lsp-ui (doc childframe, sideline, peek) which
;; ef-themes does not cover
(load "~/.emacscore/themes/ef-day/lsp-ui-ef-day-style.el")
