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
  (setq org-format-latex-options
        (plist-put org-format-latex-options :scale 2.0))
(when (equal current-theme 'nord)
  (setq org-format-latex-options
        (plist-put org-format-latex-options :foreground "White")))
