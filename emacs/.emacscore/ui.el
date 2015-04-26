;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; indent
(align-newline-and-indent)
;; Highlight current line
(global-hl-line-mode 1)

;;
;; Hide all components which i don't use
;;
;; hide scroll bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
; hide tool bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; hide menu bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))

;;
;; Set up cursor type and cursor color
;;
(blink-cursor-mode 0)
(setq-default cursor-type '(bar . 1))
(setq cursor-type 'bar)
(set-cursor-color "#ffffff")

;;
;; Set up line numbers
;;
(require 'linum)
(global-linum-mode 1)
(setq linum-format " %d")
(add-hook 'prog-mode-hook 'linum-mode)

;;
;; Tabs and spaces options
;;
(setq-default tab-width 4)

;;
;; highlight search
;;
(setq search-highlight             t)
(setq query-replace-highlight      t)
(defvar mouse-sel-retain-highlight t)

;;
;; Remove splash screen
;;
(setq inhibit-splash-screen t)

;;
;; highlight brackets
;;
(require 'paren)
(setq show-paren-style 'expression)
(show-paren-mode 2)

;;
;; electiric pair mode
;;
(electric-pair-mode)

(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\})
			    (?\' . ?\')
			    (?\< . ?\>)
                            (?\`  . ?\`)))

;;
;; Hightlight parenthes inside it
;;
(require 'highlight-parentheses)

;;
;; Set font
;;
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :height 150 :font "DejaVu Sans Mono"))

;;
;; Monokai theme
;;
(load "~/.emacs.d/lisp/themes/monokai.el")
(enable-theme 'monokai)

;; region color
(set-face-attribute 'region nil :background "#2a3a3a")
;; string line
(set-face-background 'hl-line "#3e4435")

(custom-set-faces
 '(show-paren-match ((t (:background "#1371ab" :foreground "white")))))

;;
;; Navigation
;;
(windmove-default-keybindings)

(add-to-list 'load-path "~/.emacs.d/nyan-mode")
(require 'nyan-mode)

;;
;; Mode line setup
;;
(setq-default
 mode-line-format
 '(; Position, including warning for 80 columns
   (:propertize "%4l:" face mode-line-position-face)
   (:eval (propertize "%2c" 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   ; emacsclient [default -- keep?]
   ;mode-line-client
   " "

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
   ; nyan-mode uses nyan cat as an alternative to %p
    "| "
   (:eval (list (nyan-create)))
   (:propertize "|" face mode-line-position-face)
   "-%-"
   ))


(display-time-mode 1)

(setq display-time-mail-string "")

;; Extra mode line faces
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(set-face-attribute 'mode-line nil
    :foreground "white" :background "black"
    :inverse-video nil
    :box '(:line-width 1 :color "gray60" :style nil))

(set-face-attribute 'mode-line-inactive nil
    :foreground "white" :background "black"
    :inverse-video nil
    :box '(:line-width 3 :color "black" :style nil))

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
;; Change separator color
;;
(custom-set-faces
 '(fringe ((t (:background "393939")))))

(set-face-attribute 'vertical-border
                    nil
                    :foreground "#393920")

;;
;; hide mode line
;;
(defun toggle-mode-line () "toggles the modeline on and off"
  (interactive)
  (setq mode-line-format
    (if (equal mode-line-format nil)
        (default-value 'mode-line-format)) )
  (redraw-display))
