;;; .emacs --- Latex and org-mode configuration  -*- lexical-binding: t -*-

(require 'org)

;; Set to nil here, because its incompatible with viewing the agenda
(setq org-startup-with-latex-preview nil)

;; Set latex live preview backend
(setq org-preview-latex-default-process 'dvisvgm)
(setq org-latex-create-formula-image-program 'dvisvgm)

;; Set Latex formatting options
(setq org-format-latex-options
        (plist-put org-format-latex-options :background nil))
;; Previews render at a fixed pt size that ignores both the frame font and
;; the panel's real pixel density, so the right :scale differs per machine.
;; Hostnames are all `fedora', so key it on the panel model sway reports
;; (`swaymsg -t get_outputs' -> "model"). Effective dvisvgm scale is this
;; times the backend's :image-size-adjust (1.7 for dvisvgm).
(defvar my-org-latex-scale-by-monitor
  '(("0x41A0" . 1.8))  ; laptop: 163 dpi 1920x1200 panel reported as 96 dpi
  "LaTeX preview :scale keyed by the sway output model emacs lives on.")

(defun my-org-latex-scale ()
  "Preview scale for this machine's panel, see `my-org-latex-scale-by-monitor'.
Unknown panels and sessions without sway fall back to following the font
size: 1.0 matched 13pt text, 22pt on the 4K panel gives ~1.7."
  (or (cdr (assoc (emacs-output-model) my-org-latex-scale-by-monitor))
      (/ (string-to-number (get-font-size)) 13.0)))

(setq org-format-latex-options
      (plist-put org-format-latex-options :scale (my-org-latex-scale)))
(when (equal current-theme 'nord)
  (setq org-format-latex-options
        (plist-put org-format-latex-options :foreground "White")))
