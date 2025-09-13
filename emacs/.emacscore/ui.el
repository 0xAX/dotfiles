;;; ui.el --- UI configuration of GNU Emacs  -*- lexical-binding: t -*-

(defun get-font-size ()
  "Return an integer font height (1/10pt) based on focused Hyprland monitor width.
Falls back to 1920 if anything fails."
  (if (and
       (string= (string-trim (or (getenv "XDG_SESSION_TYPE") "")) "wayland")
       (getenv "HYPRLAND_INSTANCE_SIGNATURE"))
      (let
	  ((screen-res (gethash
			"res"
			(json-parse-string
			 (shell-command-to-string "hyprctl monitors -j | jq '.[] | select(.focused==true) | {name: .name, res: .width, height: .height, scale: .scale}'")) 1920)))
	(if (> screen-res 1920)
	    "22"
	  "13"))
    "13"))

;; yes-or-no-p to y-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable text wrapping
(global-visual-line-mode)

;; do not remove new line at the end of buffer
(setq mode-require-final-newline t)

;; Disable system beep
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Set fonts
(cond ((file-directory-p "/usr/share/fonts/fira-code")
       (progn
         (set-frame-font (concat "Fira Code-" (get-font-size)))
         (enable-ligatures)))
      (t
       (set-frame-font (concat "Monospace-" (get-font-size)))))

;; display file name in title
(setq-default frame-title-format
	      (list '((buffer-file-name " %f"
                                        (dired-directory dired-directory
                                                         (revert-buffer-function " %b" ("%b – Dir:  " default-directory)))))))

;; Set-up Emacs theme. The choice depends on the `current-theme' variable.
;; This variable must be defined in the beginning of the .emacs
(cond
 ((equal current-theme 'material)
  (progn
    (setq material-theme-path
          (concat user-emacs-directory "/straight/build/material-theme"))
    (load (concat material-theme-path "/material-light-theme.el"))
    (when (equal tab-mode 'tabbar)
      (load "~/.emacscore/themes/material/tabbar.el"))
    (set-face-attribute 'show-paren-match-expression nil :background "#90A4AE")
    (enable-theme 'material-light)))
 ((equal current-theme 'capptuccin-machiato)
  (progn
    (setq capptuccin-theme-path
        (concat user-emacs-directory "/straight/build/catppuccin-theme"))
    (load (concat capptuccin-theme-path "/catppuccin-theme.el"))
    (setq catppuccin-flavor 'macchiato)
    (when (equal tab-mode 'tabbar)
      (load "~/.emacscore/themes/catppuccin/tabbar-catppuccin-style.el"))
    (enable-theme 'catppuccin)))
 ((equal current-theme 'capptucin-mocha)
  (progn
    (setq capptuccin-theme-path
        (concat user-emacs-directory "/straight/build/catppuccin-theme"))
    (load (concat capptuccin-theme-path "/catppuccin-theme.el"))
    (setq catppuccin-flavor 'mocha)
    (when (equal tab-mode 'tabbar)
      (load "~/.emacscore/themes/catppuccin/tabbar-catppuccin-style.el"))
    (enable-theme 'catppuccin)))
 ((equal current-theme 'gruvbox)
  (progn
    (setq gruvbox-theme-path
        (concat user-emacs-directory "/straight/build/gruvbox-theme"))
    (load (concat gruvbox-theme-path "/gruvbox-theme.el"))
    (load (concat gruvbox-theme-path "/gruvbox-dark-hard-theme.el"))
    (when (equal tab-mode 'tabbar)
      (load "~/.emacscore/themes/gruvbox/tabbar-gruvbox-style.el"))
    (enable-theme 'gruvbox-dark-hard)))
 ((equal current-theme 'nord)
  (progn
    (load "~/.emacscore/lisp/nord-theme.el")
    (enable-theme 'nord)))
 (t
  (progn
    (setq solarized-theme-path
          (concat user-emacs-directory "/straight/build/solarized-emacs"))
    (load (concat solarized-theme-path "/solarized-palettes.el"))
    (load (concat solarized-theme-path "/solarized-faces.el"))
    (load (concat solarized-theme-path "/solarized.el"))
    (load (concat solarized-theme-path "/solarized-theme.el"))
    (load (concat solarized-theme-path "/solarized-light-theme.el"))
    (setq x-underline-at-descent-line t)
    (enable-theme 'solarized-light)
    (when (equal tab-mode 'tabbar)
      (load "~/.emacscore/themes/solarized/tabbar-solarized-light-style.el")))))

;; hide scroll bar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
; hide tool bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;; hide menu bar
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; Disable scroll
(put 'scroll-left 'disabled nil)
;; do not show splash screen
(setq inhibit-splash-screen t)

;; Set up cursor type and cursor color
(blink-cursor-mode 0)
(setq-default cursor-type 'bar)

;; Set up line numbers
(if (< emacs-major-version 29)
    (progn
      (require 'linum)
      (global-linum-mode 1)
      (setq linum-format " %d "))
    (global-display-line-numbers-mode 1))

;; Highlight current line
(global-hl-line-mode 1)

;; highlight search
(setq search-highlight t)
(setq query-replace-highlight t)

;;
;; highlight and pair-brackets brackets
;;
(require 'paren)
(setq show-paren-style 'expression)
(show-paren-mode t)

;; auto complete
(electric-pair-mode 1)
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\})
                            (?\' . ?\')
			    (?\& . ?\&)
			    (?\| . ?\|)
                            (?\`  . ?\`)))

;; Navigation
(windmove-default-keybindings)

;; Hide sparators
(fringe-mode '(0 . 0))

;; auto complete mini buffer
(icomplete-mode t)

;; CUA mode
(cua-mode t)
(setq cua-auto-tabify-rectangles nil)
(transient-mark-mode 1)
(setq cua-keep-region-after-copy t)

;; do not store ac-comphist.dat in ~/.emacs.d/
(when (require 'auto-complete-config nil 'noerror)
  (setq ac-comphist-file  "~/.cache/ac-comphist.dat")
  (ac-config-default))

;; Enable column number in mode-line
(setq column-number-mode t)

;; set standard tab width
(setq tab-width 8)

;; Ivy configuration
(ivy-mode)
(ivy-posframe-mode)

;; If we switch to help window, move cursor there
(setq help-window-keep-selected t)

;; Customize colors of  centaur-tabs
(when (and (not (equal tab-mode 'tabbar)) (equal current-theme 'nord))
  (custom-set-faces
   `(centaur-tabs-default ((t (:background ,"#434c5e" :foreground ,"#eceff4" :box nil))))
   `(centaur-tabs-selected ((t (:background ,"#434c5e" :foreground ,"#d8dee9" :box nil))))
   `(centaur-tabs-unselected ((t (:background ,"#2e3440" :foreground ,"#d8dee9" :box nil))))
   `(centaur-tabs-selected-modified ((t (:background ,"#434c5e" :foreground ,"#eceff4" :box nil))))
   `(centaur-tabs-unselected-modified ((t (:background ,"#434c5e" :foreground ,"#eceff4" :box nil))))
   `(centaur-tabs-modified-marker-selected ((t (:inherit 'centaur-tabs-selected-modified :foreground ,"#d8dee9" :box nil))))
   `(centaur-tabs-active-bar-face ((t (:background ,"#88c0d0" :foreground ,"#88c0d0"))))
   `(centaur-tabs-modified-marker-unselected ((t (:inherit 'centaur-tabs-unselected-modified :background "#434c5e" :foreground ,"#d8dee9" :box nil))))))

;; Change foreground for mode-line if theme is set to nord
(when (equal current-theme 'nord)
  (custom-set-faces
   `(mode-line ((t (:foreground ,"#D8DEE9" :background ,"#4C566A"))))
   `(font-lock-comment-face ((,t (:foreground ,"#72809a"))))
   `(font-lock-comment-delimiter-face ((,t (:foreground ,"#72809a"))))
   `(line-number ((,t (:foreground ,"#72809a"))))))

;; Load tabs based on centaur-tabs if it is enabled
(when (not (equal tab-mode 'tabbar))
  (load "~/.emacscore/tabs.el"))
