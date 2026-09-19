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
  ;; Effective dvisvgm scale is this times the backend's :image-size-adjust
  ;; (1.7 for dvisvgm), so 1.0 here matches Org's default sizing.
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 1.0))
(when (equal current-theme 'nord)
  (setq org-format-latex-options
        (plist-put org-format-latex-options :foreground "White")))
