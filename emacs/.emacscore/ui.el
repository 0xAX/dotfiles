;; ui.el --- UI configuration of GNU Emacs  -*- lexical-binding: t -*-

(defun emacs-output-width ()
  "Width in pixels of the screen emacs lives on, or nil if it cannot be told.
Emacs always ends up on the big panel: workspace 2 is pinned to it and a
window rule moves emacs there, in both the sway and the Hyprland config.  So
the screen to measure is the widest one attached - not the focused one, which
at startup is still whichever screen the launcher was invoked from.

Asks whichever wayland compositor is running - sway via swaymsg, Hyprland via
hyprctl -- and parses the JSON here rather than shelling out to jq."
  (let ((cmd (cond ((getenv "SWAYSOCK") "swaymsg -t get_outputs -r")
		   ((getenv "HYPRLAND_INSTANCE_SIGNATURE") "hyprctl monitors -j"))))
    (when cmd
      (ignore-errors
	(let ((widths (mapcar
		       (lambda (o)
			 ;; sway nests the geometry in `rect', hyprland keeps it flat
			 (or (alist-get 'width (alist-get 'rect o))
			     (alist-get 'width o)
			     0))
		       (append (json-parse-string (shell-command-to-string cmd)
						  :object-type 'alist)
			       nil))))
	  (when widths
	    (apply #'max widths)))))))

(defun get-font-size ()
  "Return the font size, in points, as a string.
13pt is unreadable on the 32\" 4K panel emacs opens on, so pick the size from
how wide that screen is.  This used to be gated on HYPRLAND_INSTANCE_SIGNATURE,
which is why sway sessions were always stuck at 13pt.  Falls back to 13
whenever no compositor can be asked."
  (let ((width (emacs-output-width)))
    (if (and width (> width 1920))
	"22"
      "13")))

;; Set up line numbers
(if (< emacs-major-version 29)
    (progn
      (require 'linum)
      (global-linum-mode 1)
      (setq linum-format " %d "))
    (global-display-line-numbers-mode 1))

;; Highlight current line
(global-hl-line-mode 1)

;; yes-or-no-p to y-n
(defalias 'yes-or-no-p 'y-or-n-p)

;; Enable text wrapping
(global-visual-line-mode)

;; do not remove new line at the end of buffer
(setq mode-require-final-newline t)

;; Disable system beep
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Set fonts. The choice depends on the `current-font' variable.
;; This variable must be defined in the beginning of the .emacs
(cond
 ((and (equal current-font 'fira-code)
       (find-font (font-spec :family "Fira Code")))
  (set-frame-font (concat "Fira Code-" (get-font-size)))
  (enable-ligatures))
 ((and (equal current-font 'jetbrains-mono)
       (find-font (font-spec :family "JetBrains Mono")))
  (set-frame-font (concat "JetBrains Mono-" (get-font-size)))
  (enable-ligatures))
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
  (load "~/.emacscore/themes/material/material.el"))
 ((equal current-theme 'catppuccin-macchiato)
  (load "~/.emacscore/themes/catppuccin/macchiato.el"))
 ((equal current-theme 'catppuccin-mocha)
  (load "~/.emacscore/themes/catppuccin/mocha.el"))
 ((equal current-theme 'gruvbox)
  (load "~/.emacscore/themes/gruvbox/gruvbox.el"))
 ((equal current-theme 'nord)
  (load "~/.emacscore/themes/nord/nord.el"))
 ((equal current-theme 'monokai)
  (load "~/.emacscore/themes/monokai/monokai.el"))
 ((equal current-theme 'ef-day)
  (load "~/.emacscore/themes/ef-day/ef-day.el"))
 (t
  (load "~/.emacscore/themes/solarized/solarized.el")))

;; draws the underline at the font's descent line (bottom) rather than the default baseline
(setq x-underline-at-descent-line t)

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

;; highlight search
(setq search-highlight t)
(setq query-replace-highlight t)

;;
;; highlight and pair-brackets brackets
;;
(require 'paren)
(setq show-paren-style 'expression)
;; The expression highlight overlay has a higher priority than the region
;; overlay and would cover the selection color, so skip paren highlighting
;; while the region is active.
(setq show-paren-data-function
      (lambda ()
        (unless (use-region-p)
          (show-paren--default))))
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

;; Load tabs based on centaur-tabs if it is enabled
(when (not (equal tab-mode 'tabbar))
  (load "~/.emacscore/tabs.el"))
