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

;;
;; auto complete mini buffer
;;
(icomplete-mode t)

;;
;; ido mode
;;
;(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)

;;
;; CUA mode
;;
(cua-mode t)
(setq cua-auto-tabify-rectangles nil)
(transient-mark-mode 1)
(setq cua-keep-region-after-copy t)

;;
;; Project explorer
;;
(add-to-list 'load-path "~/.emacs.d/neotree")
; neotree config
(setq neo-tree-file "~/.emacscore/.neotree")
(require 'neotree)

;;
;; Tabbar mode
;;
(add-to-list 'load-path "~/.emacs.d/tabbar")
(require 'tabbar)

;;
;; All tabs in one group
;;
(defun my-tabbar-buffer-groups ()
  (list (cond
         ((string-equal ".h"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 2)
                                   (length (buffer-name)))) "system")
         ((string-equal ".c"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 2)
                                   (length (buffer-name)))) "system")
         ((string-equal ".asm"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 4)
                                   (length (buffer-name)))) "system")
         ((string-equal ".S"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 2)
                                   (length (buffer-name)))) "system")
         ((string-equal ".S"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 2)
                                   (length (buffer-name)))) "system")
         ((string-equal ".ld"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 3)
                                   (length (buffer-name)))) "system")
         ((string-equal "Makefile"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 8)
                                   (length (buffer-name)))) "system")
         ((string-equal "Kbuild"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 6)
                                   (length (buffer-name)))) "system")
         ((string-equal "*t"
                        (substring (buffer-name) 0 2)) "terminal")
         ((string-equal ".el"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 3)
                                   (length (buffer-name)))) "elisp")
         ((string-equal ".emacs"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 6)
                                   (length (buffer-name)))) "elisp")
         ((string-equal ".erl"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 4)
                                   (length (buffer-name)))) "erlang")
         ((string-equal ".hrl"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 4)
                                   (length (buffer-name)))) "erlang")
         ((string-equal ".js"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 3)
                                   (length (buffer-name)))) "web")
         ((string-equal ".html"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 5)
                                   (length (buffer-name)))) "web")
         ((string-equal ".tpl.html"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 9)
                                   (length (buffer-name)))) "web")
         ((string-equal ".css"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 4)
                                   (length (buffer-name)))) "web")
         ((string-equal ".go"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 3)
                                   (length (buffer-name)))) "golang")
         ((string-equal ".sh"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 3)
                                   (length (buffer-name)))) "system")
         ((string-equal ".cpp"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 4)
                                   (length (buffer-name)))) "system")
         ((string-equal "*magit"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 6)
                                   (length (buffer-name)))) "git")
         ((string-equal "*"
                        (substring (buffer-name) 0 1))
          "emacs")

         ((eq major-mode 'dired-mode) "Dired")
         (t "user"))))

;; set up custom tabbar group
(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

;; tabbar background
(setq tabbar-background-color "#333333")

;;
;; selected tabbar
;;
(set-face-attribute
 'tabbar-selected nil
 :background "dim gray"
 :foreground "bold gray"
 :family "Droid Sans Mono"
 :box '(:line-width 1 :color "gray60" :style nil))

;;
;; unselected tabbar
;;
(set-face-attribute
 'tabbar-unselected nil
 :background "#333333"
 :foreground "white"
 :box '(:line-width 1 :color "#333333" :style nil))

;;
;; tabbar separator color
;;
(set-face-attribute
 'tabbar-separator nil
 :background "gray20"
 :height 0.6)

;;
;; tabbar buttons
;;
(set-face-attribute
 'tabbar-button nil
 :background "#333333"
 :box '(:line-width 1 :color "#333333" :style nil))

;;
;; Enable tabbar mode
;;
(tabbar-mode)

;;
;; Dired configuration
;;
(require 'dired-x)
(require 'dired)
(put 'dired-find-alternate-file 'disabled nil)

;;
;; auto complete
;;
(add-to-list 'load-path "~/.emacs.d/popup")
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "/.emacs.d/auto-complete")

;;
;; Enable rainbow mode
;;
(add-to-list 'load-path "~/.emacs.d/rainbow")
(require 'rainbow-mode)
(rainbow-mode 1)

;;
;; Enable pos-tip for auto-complete rendering
;;
(require 'pos-tip)
