;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        Useful modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
		 ;; C/C++/Asm/ld/Makefile, KBuild
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
		 ;; terminal
		 ((string-equal "*t"
		 				(substring (buffer-name) 0 2)) "terminal")
		 ;; emacs lisp
		 ((string-equal ".el"
						(substring (buffer-name)
								   (- (length (buffer-name)) 3)
								   (length (buffer-name)))) "elisp")
		 ((string-equal ".emacs"
						(substring (buffer-name)
								   (- (length (buffer-name)) 6)
								   (length (buffer-name)))) "elisp")
		 ;; erlang
		 ((string-equal ".erl"
						(substring (buffer-name)
								   (- (length (buffer-name)) 4)
								   (length (buffer-name)))) "erlang")
 		 ((string-equal ".hrl"
						(substring (buffer-name)
								   (- (length (buffer-name)) 4)
								   (length (buffer-name)))) "erlang")
		 ;; web
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
		 ;; golang
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
		 ;; file manager
		 ((eq major-mode 'dired-mode) "dired")

		 ;; git
		 ((string-equal "*magit"
		 				(substring (buffer-name)
		 						   (- (length (buffer-name)) 6)
		 						   (length (buffer-name)))) "git")
		 ;; emacs files
		 ((string-equal "*"
						(substring (buffer-name) 0 1))
		  "emacs")
		 ;; rest
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
