;;
;; Monokai theme
;;
(if (display-graphic-p)
    (load "~/.emacs.d/themes/monokai.el")
  (load "~/.emacs.d/themes/smyx.el"))
(if (display-graphic-p)
    (enable-theme 'monokai)
  (enable-theme 'smyx))

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
(set-cursor-color "white")

;;
;; Set up line numbers
;;
(require 'linum)
(global-linum-mode 1)
(setq linum-format " %d ")

;;
;; highlight search
;;
(setq search-highlight             t)
(setq query-replace-highlight      t)

;;
;; remove splash screen
;;
(setq inhibit-splash-screen t)

;;
;; highlight and pair-brackets brackets
;;
(require 'paren)
(setq show-paren-style 'expression)
(show-paren-mode 2)
(electric-pair-mode)
(setq electric-pair-pairs '(
                            (?\" . ?\")
                            (?\{ . ?\})
                            (?\' . ?\')
                            (?\< . ?\>)
                            (?\`  . ?\`)))

;;
;; Set font
;;
(when (member "DejaVu Sans Mono" (font-family-list))
  (set-face-attribute 'default nil :height 150 :font "DejaVu Sans Mono"))

;; region color
;; previously it was #2a3a3a
(set-face-attribute 'region nil :background "grey10")

;;
;; Set fonts
;;
(custom-set-faces
 '(show-paren-match ((t (:background "#116599" :foreground "white")))))

;;
;; Navigation
;;
(windmove-default-keybindings)

;;
;; Hide sparators
;;
(fringe-mode '(0 . 0))

;;
;; auto complete mini buffer
;;
(icomplete-mode t)

;;
;; ido mode
;;
(custom-set-variables
 '(ido-enable-last-directory-history nil)
 '(ido-record-commands nil)
 '(ido-max-work-directory-list 0)
 '(ido-max-work-file-list 0))

(require 'ido)
(setq ido-save-directory-list-file nil)
(setq ido-enable-flex-matching t)
(ido-mode 1)
(ido-everywhere 1)

;;
;; CUA mode
;;
(cua-mode t)
(setq cua-auto-tabify-rectangles nil)
(transient-mark-mode 1)
(setq cua-keep-region-after-copy t)

;;
;; Dired configuration
;;
(require 'dired-x)
(require 'dired)
(setq dired-listing-switches "-lapq --author --file-type --group-directories-first
                              -h --human-readable --hide='*.o' ")

;;
;; auto complete
;;
(add-to-list 'load-path "~/.emacs.d/popup")
(add-to-list 'load-path "~/.emacs.d/auto-complete")
(require 'auto-complete-config)
(ac-config-default)
(add-to-list 'ac-dictionary-directories "/.emacs.d/auto-complete")

;;
;; Enable pos-tip for auto-complete rendering
;;
(require 'pos-tip)

;;
;; Great help sometimes
;;
(add-to-list 'load-path "~/.emacs.d/discover-my-major")
(require 'discover-my-major)

;; Highlight current line
(global-hl-line-mode 1)

;; string line
(set-face-background 'hl-line "gray30")

;; indentation
(align-newline-and-indent)
