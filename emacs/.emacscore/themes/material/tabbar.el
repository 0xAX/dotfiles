;;; material.el --- GNU Emacs tabbar styles for material theme  -*- lexical-binding: t -*-

;; Tabbar mode
(require 'tabbar)

;; Group similar files together in tabbar
(defun my-tabbar-buffer-groups ()
  (list (cond
         ;; Emacs buffers
         ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
         ((eq major-mode 'dired-mode) "emacs")

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

         ;; lisp programming
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

         ;; golang programming
         ((string-equal "go"       (file-name-extension (buffer-name))) "golang")

         ;; documentation
         ((string-equal "md"       (file-name-extension (buffer-name))) "documentation")
         ((string-equal "rst"      (file-name-extension (buffer-name))) "documentation")
         ((string-equal "org"      (file-name-extension (buffer-name))) "documentation")

         ;; everything else
         (t "user"))))

;; set up custom tabbar group
(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

;; tabbar background
(setq tabbar-background-color "#FAFAFA")

;; use custom faces as default one inherits font-size and other
;; things from other configuration
(custom-set-faces
 '(tabbar-default ((t (:background "#FAFAFA" :foreground "#eee8d5")))
 '(tabbar-selected ((t (:background "#ECEFF1" :foreground "#000000"))))
 '(tabbar-unselected ((t (:background "#ECEFF1" :foreground "#93a1a1"))))
 '(tabbar-modified ((t (:background "#AAAAAA" :foreground "#d33682"))))
 '(tabbar-separator ((t (:background "#ECEFEA" :foreground "#AAAAAA")))))

;; Set font for the tabbar
;; NOTE: we do it here but not in the custom-set-faces above because the custom-set-faces must have already evaluated
;; value of the font
(set-face-attribute 'tabbar-default nil :font (plist-get (font-face-attributes (face-attribute 'default :font)) :family))

;;
;; Hide tabbar buttons
;;
(setq tabbar-hide-header-button t)
(setq tabbar-use-images nil)

(customize-set-variable 'tabbar-scroll-right-button '(("") ""))
(customize-set-variable 'tabbar-scroll-left-button '(("") ""))
(customize-set-variable 'tabbar-buffer-home-button '(("") ""))

;; enable tabbar mode
(tabbar-mode)
