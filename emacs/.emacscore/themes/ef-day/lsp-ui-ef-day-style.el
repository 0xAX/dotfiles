;;; lsp-ui-ef-day-style.el --- lsp-ui styling for ef-day theme  -*- lexical-binding: t -*-

;; ef-themes deliberately does not theme lsp-ui, so the doc childframe,
;; sideline and peek windows fall back to lsp-ui's hardcoded light-theme
;; defaults (grey #b3b3b3 doc background, "deep sky blue" header, white
;; frame border) which clash with ef-day's warm palette.
;;
;; ef-day palette:
;;   bg-main #fff5ea  bg-dim #f2e9db  bg-alt #e9e0d8  bg-active #c9c0b8
;;   bg-hl-line #f9e2b2  bg-region #f0d2df  border #c8bdb6
;;   fg-main #584141  fg-dim #63728f
;;   blue #375cc6  red #ba2d2f  yellow-warmer #b75515  bg-yellow-intense #efbf00

;; Border color of the documentation childframe
(setq lsp-ui-doc-border "#c8bdb6")

(custom-set-faces
 ;; documentation childframe
 `(lsp-ui-doc-background ((t (:background "#f2e9db"))))
 `(lsp-ui-doc-header ((t (:background "#f9e2b2" :foreground "#584141" :weight bold))))
 `(lsp-ui-doc-highlight-hover ((t (:background "#f0d2df"))))
 ;; sideline
 `(lsp-ui-sideline-symbol ((t (:foreground "#63728f" :box (:line-width -1 :color "#c8bdb6") :height 0.99))))
 `(lsp-ui-sideline-current-symbol ((t (:foreground "#375cc6" :weight bold :box (:line-width -1 :color "#375cc6") :height 0.99))))
 `(lsp-ui-sideline-code-action ((t (:foreground "#b75515"))))
 `(lsp-ui-sideline-symbol-info ((t (:foreground "#63728f" :slant italic :height 0.99))))
 ;; peek (references/definitions)
 `(lsp-ui-peek-peek ((t (:background "#f2e9db"))))
 `(lsp-ui-peek-list ((t (:background "#e9e0d8"))))
 `(lsp-ui-peek-filename ((t (:foreground "#375cc6" :weight bold))))
 `(lsp-ui-peek-line-number ((t (:foreground "#63728f"))))
 `(lsp-ui-peek-highlight ((t (:background "#efbf00" :foreground "#584141" :box (:line-width -1 :color "#b75515")))))
 `(lsp-ui-peek-header ((t (:background "#c9c0b8" :foreground "#584141" :weight bold))))
 `(lsp-ui-peek-footer ((t (:background "#c9c0b8"))))
 `(lsp-ui-peek-selection ((t (:background "#f0d2df" :foreground "#584141")))))
