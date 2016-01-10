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

;; string line
(set-face-background 'hl-line "gray25")

;; region color
(set-face-attribute 'region nil :background "#2a3a3a")

(custom-set-faces
 '(show-paren-match ((t (:background "#116599" :foreground "white")))))

;;
;; Navigation
;;
(windmove-default-keybindings)

;;
;; Change separator color
;;
(custom-set-faces
 '(fringe ((t (:background "#272822")))))

(set-face-attribute 'vertical-border
                    nil
                    :foreground "#393920")

;;
;; auto complete mini buffer
;;
(icomplete-mode t)

;;
;; ido mode
;;
(add-to-list 'load-path "~/.emacs.d/ido-vertical-mode.el/")
(custom-set-variables
 '(ido-enable-last-directory-history nil)
 '(ido-record-commands nil)
 '(ido-max-work-directory-list 0)
 '(ido-max-work-file-list 0))

;;(require 'ido-vertical-mode)
(require 'ido)
(setq ido-save-directory-list-file nil)
(setq ido-enable-flex-matching t)
;;(setq ido-vertical-define-keys 'C-n-and-C-p-only)

(ido-mode 1)
(ido-everywhere 1)
;;(ido-vertical-mode 1)

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
;(put 'dired-find-alternate-file 'disabled nil)
(setq dired-listing-switches "-lapq --author --file-type --group-directories-first -h --human-readable
                              --hide='*.o' ")

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

;;
;; Great help sometimes
;;
(add-to-list 'load-path "~/.emacs.d/discover-my-major")
(require 'discover-my-major)

;; Highlight current line
(global-hl-line-mode 1)

;; indent
(align-newline-and-indent)
