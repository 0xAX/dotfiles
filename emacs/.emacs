;; package  --- .emacs

;;; Commentary:

;;
;;        DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;                Version 2, December 2004
;;
;; Everyone is permitted to copy and distribute verbatim or modified
;; copies of this license document, and changing it is allowed as long
;; as the name is changed.
;;
;;         DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;; TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
;;
;;  0. You just DO WHAT THE FUCK YOU WANT TO.
;;
;;
;;
;;                                                            0xAX :)
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        Standard settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/lisp")

;;
;; Measure startup time
;;
(require 'init-benchmarking)

;;
;; discover major mode help
;;
(load "~/.emacs.d/lisp/discover-major.el")
(require 'discover-my-major)

;;
;; Load emacs in fullscreen
;;
(defun fullscreen ()
       "Load Emacs in fullscreen mode at startup."
       (interactive)
       (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                 '(2 "_NET_WM_STATE_FULLSCREEN" 0)))

(fullscreen)

;;
;; Delete text in selection mode when typing
;;
(delete-selection-mode 1)

;;
;; Current locale
;;
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(setq current-language-environment "UTF-8")

;;
;; bash is a standard shell
;;
(setq explicit-shell-file-name "/bin/bash")

;;
;; Prevent creation of backup files
;;
(setq make-backup-files         nil)
(setq auto-save-list-file-name  nil)
(setq auto-save-default         nil)

;;
;; Undo/Redo
;;
(require 'undo-tree)
(global-undo-tree-mode 1)

;;
;; Disable startup emacs window
;;
(defvar inhibit-start-screen 1)
(defvar inhibit-splash-screen 1)
(defvar inhibit-startup-message t)
(defvar initial-scratch-message 0)

;;
;; yes-or-no-p to y-n
;;
(defalias 'yes-or-no-p 'y-or-n-p)

;;
;; Disable system beep
;;
(defvar visual-bell t)

;;
;; Save last emacs session
;;
(require 'desktop)
(desktop-save-mode 1)
(setq history-length 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        Package manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (>= emacs-major-version 25)
  (require 'package)
  (package-initialize)
    (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
    (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
    (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        Load extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; ui
;;
(load "~/.emacscore/utils.el")
(load "~/.emacscore/uimodes.el")
(load "~/.emacscore/ui.el")






;; load keybindings
(load "~/.emacscore/keybindings.el")

;;
;; markups
;;
(load "~/.emacscore/markups.el")

;;
;; load development modes
;;
(load "~/.emacscore/dev/web.el")
(load "~/.emacscore/dev/golang.el")
(load "~/.emacscore/dev/erlang.el")
(load "~/.emacscore/dev/nasm.el")
(load "~/.emacscore/dev/c.el")
(load "~/.emacscore/dev/sed.el")
(load "~/.emacscore/dev/elixir.el")

;; load build modes
(load "~/.emacscore/build/make.el")

;; git
(load "~/.emacscore/vcs/git.el")

;; rfc mode
(load "~/.emacs.d/lisp/rfc.el")
(require 'irfc)

;;
;; some utils
;;
(defadvice ansi-term (after advise-ansi-term-coding-system activate)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

;(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; close the terminal buffer automatically on exit
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg) activate)
  (if (memq (process-status proc) '(signal exit))
	  (let ((buffer (process-buffer proc)))
		ad-do-it
		(kill-buffer buffer))
	ad-do-it))

;; Ok it's done
(message "All done, %s%s" (user-login-name) ".")
(message "Don't forget to check mail and rss-feed")
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)
