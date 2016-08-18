;;
;; Monokai theme
;;
(if (display-graphic-p)
        ;; load and configure material theme
        (progn (load "~/.emacscore/themes/material.el")
               (enable-theme 'material)
               (load "~/.emacscore/themes/helpers/tabbar-material-style.el")
               (load "~/.emacscore/themes/helpers/mode-line-material.el"))
        ;; load and configure smyx
        (progn (load "~/.emacscore/themes/smyx.el")
               (enable-theme 'smyx)
               (load "~/.emacscore/themes/helpers/tabbar-monokai-style.el")
               (load "~/.emacscore/themes/helpers/mode-line-monokai.el"))
        )

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
(setq-default cursor-type 'bar)

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
(when (member "Inconsolata-10, Monaco-5, Consolas, 'Courier New', Courier" (font-family-list))
  (set-face-attribute 'default nil :height 120 :font "Inconsolata-40, Monaco, Consolas, 'Courier New', Courier"))
(set-default-font "Inconsolata-15")

;(when (member "DejaVu Sans Mono" (font-family-list))
;  (set-face-attribute 'default nil :height 150 :font "DejaVu Sans Mono"))

;; region color
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


;; indentation
;(align-newline-and-indent)
