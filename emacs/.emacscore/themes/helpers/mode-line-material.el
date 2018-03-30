;;; mode-line-material.el --- GNU Emacs modeline styles for material theme  -*- lexical-binding: t -*-
;;
;; Author:  Alexander Kuleshov <kuleshovmail@gmail.com>
;; URL:     https://github.com/0xAX/med
;;
;; License: See LICENSE file.

;; Mode line setup
(setq-default
 mode-line-format
 '(; Position, including warning for 80 columns
   (:propertize "%4l:" face mode-line-position-face)
   (:eval (propertize "%2c " 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   "| "

   ; directory and buffer/file name
   (:propertize "%b"
                face mode-line-filename-face)

   ; narrow [default -- keep?]
   "%n |"
   ; mode indicators: vc, recursive edit, major mode, minor modes, process, global
   (vc-mode vc-mode)
   ;; mode name
   " %["
   (:propertize mode-name
                face mode-line-mode-face)
   ;; for time
   " |"
   ;;
   "%] "
   (global-mode-string global-mode-string)
   " | "
   "["
    (:propertize "%p" 'face 'font-lock-constant-face) ;; % above top
    "/"
    (:propertize "%I" 'face 'font-lock-constant-face) ;; size
    "] "
   (:propertize "|" face mode-line-position-face)

   "-%-"
   ))

(display-time-mode 1)

(make-face 'mode-line)
(make-face 'mode-line-position-face)

(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(set-face-attribute 'mode-line nil
    :foreground "white" :background "#263238"
    :inverse-video nil
    :box '(:line-width 1 :color "#263238" :style nil))

(set-face-attribute 'mode-line-inactive nil
    :foreground "white" :background "#263238"
    :inverse-video nil
    :box '(:line-width 3 :color "#263238" :style nil))

(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground "gray60")

(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "orange"
    :weight 'bold)

(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face
    :family "Menlo" :height 150)

(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "white")

(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "gray40"
    :height 150)

(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground "white")

(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground "black" :background "#eab700")

;;
;; hide mode line
;;
(defun toggle-mode-line () "toggles the modeline on and off"
  (interactive)
  (setq mode-line-format
    (if (equal mode-line-format nil)
        (default-value 'mode-line-format)) )
  (redraw-display))
