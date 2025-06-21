;;; tabs.el --- Configuration of the tabs based on centaur-tab  -*- lexical-binding: t -*-

(require 'centaur-tabs)

;; Enable uniform appearance for centaur tabs
(centaur-tabs-headline-match)

(setq
 ;; Set height of the tabs
 centaur-tabs-height 34
 ;; Set form of the tabs
 centaur-tabs-style "box"
 ;; Enable icons in the tabs
 centaur-tabs-set-icons t
 ;; Set the icons style in the tabs
 centaur-tabs-icon-type 'nerd-icons
 ;; Set the position of the mark for an active bar
 centaur-tabs-set-bar 'left
 ;; Enable modified marker
 centaur-tabs-set-modified-marker t
 ;; Set modified marker
 centaur-tabs-modified-marker "*")

;; Font-size
(centaur-tabs-change-fonts (face-attribute 'default :font) 130)

(defun centaur-tabs-buffer-groups ()
  "`centaur-tabs-buffer-groups' control buffers' group rules."
  (list
   (cond
    ;; Emacs tab bar group
    ((or
      (string-equal "*" (substring (buffer-name) 0 1))
      (memq major-mode '(magit-process-mode
                         magit-status-mode
                         magit-diff-mode
                         magit-log-mode
                         magit-file-mode
                         magit-blob-mode
                         magit-blame-mode
                         dired-mode
                         help-mode
                         helpful-mode
                         )))
     "Emacs")

    ;; system programming files
    ((string-equal "sh"       (file-name-extension (buffer-name))) "system")
    ((string-equal "h"        (file-name-extension (buffer-name))) "system")
    ((string-equal "c"        (file-name-extension (buffer-name))) "system")
    ((string-equal "cc"       (file-name-extension (buffer-name))) "system")
    ((string-equal "hh"       (file-name-extension (buffer-name))) "system")
    ((string-equal "cpp"      (file-name-extension (buffer-name))) "system")
    ((string-equal "hpp"      (file-name-extension (buffer-name))) "system")
    ((string-equal "S"        (file-name-extension (buffer-name))) "system")
    ((string-equal "s"        (file-name-extension (buffer-name))) "system")
    ((string-equal "asm"      (file-name-extension (buffer-name))) "system")
    ((string-equal "ld"       (file-name-extension (buffer-name))) "system")
    ((string-equal "mk"       (file-name-extension (buffer-name))) "system")
    ((string-equal "inc"      (file-name-extension (buffer-name))) "system")
    ((string-equal "KConfig"  (buffer-name)) "system")
    ((string-equal "Kconfig"  (buffer-name)) "system")
    ((string-equal "Kbuild"   (buffer-name)) "system")
    ((string-equal "Makefile" (buffer-name)) "system")

    ;; Org mode buffers
    ((memq major-mode '(org-mode
                        org-agenda-clockreport-mode
                        org-src-mode
                        org-agenda-mode
                        org-beamer-mode
                        org-indent-mode
                        org-bullets-mode
                        org-cdlatex-mode
                        org-agenda-log-mode
                        diary-mode))
     "OrgMode")

    ;; Lisp buffers
    ((string-equal "el"       (file-name-extension (buffer-name))) "lisp")
    ((string-equal "lisp"     (file-name-extension (buffer-name))) "lisp")
    ((string-equal "emacs"    (buffer-name)) "lisp")

    ;; erlang programming
    ((string-equal "erl"      (file-name-extension (buffer-name))) "erlang")
    ((string-equal "ex"       (file-name-extension (buffer-name))) "erlang")
    ((string-equal "exs"      (file-name-extension (buffer-name))) "erlang")
    ((string-equal "hrl"      (file-name-extension (buffer-name))) "erlang")

    ;; web proggramming
    ((string-equal "json"     (file-name-extension (buffer-name))) "web")
    ((string-equal "js"       (file-name-extension (buffer-name))) "web")
    ((string-equal "html"     (file-name-extension (buffer-name))) "web")
    ((string-equal "css"      (file-name-extension (buffer-name))) "web")
    ((string-equal "tpl.html" (file-name-extension (buffer-name))) "web")
    ((string-equal "sys.mjs"  (file-name-extension (buffer-name))) "web")

    ;; golang programming
    ((string-equal "go"       (file-name-extension (buffer-name))) "golang")

    ;; documentation
    ((string-equal "md"       (file-name-extension (buffer-name))) "documentation")
    ((string-equal "rst"      (file-name-extension (buffer-name))) "documentation")
    ((string-equal "org"      (file-name-extension (buffer-name))) "documentation")

    ;; Other tabs
    (t
     (centaur-tabs-get-group-name (current-buffer))))))

;; Enable centaur tabs
(centaur-tabs-mode)
