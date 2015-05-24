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
;; Some utils
;;
(require 'init-benchmarking)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        Package manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        Load extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; ui
;;
(load "~/.emacscore/utils.el")
(load "~/.emacscore/ui.el")

(load "~/.emacscore/keybindings.el")
(load "~/.emacscore/markups.el")
(load "~/.emacscore/org/orgmode.el")
(load "~/.emacscore/dev/web.el")
(load "~/.emacscore/dev/golang.el")
(load "~/.emacscore/dev/erlang.el")
(load "~/.emacscore/dev/nasm.el")
(load "~/.emacscore/dev/c.el")
(load "~/.emacscore/dev/sed.el")
(load "~/.emacscore/dev/elixir.el")
(load "~/.emacscore/dev/shell.el")
(load "~/.emacscore/build/make.el")
(load "~/.emacscore/vcs/git.el")
(load "~/.emacscore/vcs/hg.el")
(load "~/.emacscore/vcs/svn.el")
(load "~/.emacscore/term.el")
(load "~/.emacs.d/lisp/rfc.el")
(require 'irfc)

(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(custom-set-variables
  '(coffee-tab-width 2)
  '(indent-tabs-mode nil)
  '(js2-basic-offset 2))

(message "All done, %s%s" (user-login-name) ".")
