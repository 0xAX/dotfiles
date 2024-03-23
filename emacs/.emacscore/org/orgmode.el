;;; .emacs --- My org-mode configuration  -*- lexical-binding: t -*-

(require 'org)

;; set path to org directory
(when (file-directory-p "~/dev/todo")
  (setq org-directory "~/dev/todo"))
(when (file-directory-p "~/disk/dev/todo")
  (setq org-directory "~/disk/dev/todo"))

;; Add ability to add closing notes to the done items
(setq org-log-done 'note)

;; enable org-mode for *.org files
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Some good defaults
(setq-default
 org-startup-indented t
 line-spacing 1
 org-src-fontify-natively t
 org-fontify-quote-and-verse-block t
 org-pretty-entities t
 org-startup-with-inline-images t
 org-hide-emphasis-markers t
 ;;org-adapt-indentation nil
 org-src-tab-acts-natively t
 org-src-preserve-indentation 2
 org-edit-src-content-indentation 2)


;; TODO
(setq org-edit-src-content-indentation 0)
(setq org-src-fontify-natively t
        org-src-window-setup 'current-window ;; edit in current window
        org-src-strip-leading-and-trailing-blank-lines t
        org-src-preserve-indentation t ;; do not put two spaces on the left
        org-src-tab-acts-natively t)


;; Enable auto-search in org-mode so any non-standard keypress in C-x C-j
;; mode will lead to search according to pressed keys
(setq org-goto-auto-isearch t)

;; Enable pretty printing of special symbols
(org-toggle-pretty-entities)

;; Enable support for org-tables everywhere
(add-hook 'message-mode-hook 'turn-on-orgtbl)

;; load additional org-mode helpers
(load "~/.emacscore/org/org-api.el")
(load "~/.emacscore/org/org-babel.el")
(load "~/.emacscore/org/org-ui.el")
(load "~/.emacscore/org/org-keybindings.el")

;; Set to nil here, because its incompatible with viewing the agenda
(setq org-startup-with-latex-preview nil)

(setq org-preview-latex-default-process 'dvisvgm)
(setq org-latex-create-formula-image-program 'dvisvgm)
(plist-put org-format-latex-options :scale 2)
(add-hook 'org-mode-hook 'org-fragtog-mode)

(plist-put org-format-latex-options :foreground nil)
(plist-put org-format-latex-options :background nil)
;; (setq org-preview-latex-process-alist
;;        '((dvipng :programs
;;          ("lualatex" "dvipng")
;;          :description "dvi > png" :message "you need to install the programs: latex and dvipng." :image-input-type "dvi" :image-output-type "png" :image-size-adjust
;;          (1.0 . 1.0)
;;          :latex-compiler
;;          ("lualatex -output-format dvi -interaction nonstopmode -output-directory %o %f")
;;          :image-converter
;;          ("dvipng -fg %F -bg %B -D %D -T tight -o %O %f"))
;;        (dvisvgm :programs
;;           ("latex" "dvisvgm")
;;           :description "dvi > svg" :message "you need to install the programs: latex and dvisvgm." :use-xcolor t :image-input-type "xdv" :image-output-type "svg" :image-size-adjust
;;           (1.7 . 1.5)
;;           :latex-compiler
;;           ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
;;           :image-converter
;;           ("dvisvgm %f -n -b min -c %S -o %O"))
;;        (imagemagick :programs
;;               ("latex" "convert")
;;               :description "pdf > png" :message "you need to install the programs: latex and imagemagick." :use-xcolor t :image-input-type "pdf" :image-output-type "png" :image-size-adjust
;;               (1.0 . 1.0)
;;               :latex-compiler
;;               ("xelatex -no-pdf -interaction nonstopmode -output-directory %o %f")
;;               :image-converter
;;               ("convert -density %D -trim -antialias %f -quality 100 %O"))))
