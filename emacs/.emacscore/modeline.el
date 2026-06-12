;;; modeline.el --- doom-modeline configuration  -*- lexical-binding: t -*-

;; A nice Doom-style mode-line. Uses `nerd-icons' (already installed) for the
;; glyphs; icons are enabled only when a Nerd Font is actually available so the
;; mode-line never renders as boxes/question marks on machines without it.

(require 'doom-modeline)

;; Enable icons only if we have a suitable Nerd Font (same check tabs.el uses).
(setq doom-modeline-icon
      (or (file-exists-p "/usr/share/fonts/fira-code/FiraCodeNerdFont-Regular.ttf")
          (file-exists-p "~/.local/share/fonts/FiraCodeNerdFont-Regular.ttf")))

(setq
 ;; Let the bar auto-size to the current font instead of a fixed pixel height,
 ;; so it looks right on both the small (13pt) and HiDPI (22pt) font setups.
 doom-modeline-height 1
 ;; A slightly chunkier accent bar on the left edge looks a bit more polished.
 doom-modeline-bar-width 6
 ;; Buffer name: show a shortened path so you can tell files apart.
 doom-modeline-buffer-file-name-style 'truncate-upto-project
 ;; Major-mode icon, colored to match the mode.
 doom-modeline-major-mode-icon t
 doom-modeline-major-mode-color-icon t
 ;; Modified / read-only state icon.
 doom-modeline-buffer-state-icon t
 doom-modeline-buffer-modification-icon t
 ;; Show the buffer encoding only when it is not the usual UTF-8/LF.
 doom-modeline-buffer-encoding t
 ;; Show the indentation style (spaces/tabs + width).
 doom-modeline-indent-info t
 ;; Show the minor modes segment (yasnippet, company, lsp, ...). Without a
 ;; manager this lists every active minor-mode lighter, so use `diminish' on
 ;; any that get too noisy, or set this back to nil to hide the segment.
 doom-modeline-minor-modes t
 ;; No word count in the mode-line.
 doom-modeline-enable-word-count nil
 ;; Integrations that match this config.
 doom-modeline-lsp t
 doom-modeline-vcs-max-length 24)

;; Turn it on.
(doom-modeline-mode 1)
