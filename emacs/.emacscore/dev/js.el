;;; js.el --- SLIME configuration -*- lexical-binding: t -*-

;; Highlight javascript files from the Mozilla Firefox source code
(add-to-list 'auto-mode-alist '("\\.sys\\.mjs\\'" . javascript-mode))

;; Set indentation for javascript
(setq js-indent-level 2)
