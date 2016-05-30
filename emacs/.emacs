;; package  --- .emacs
;;
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

(desktop-save-mode 0)

;;
;; Delete text in selection mode when typing
;;
(delete-selection-mode 1)

;;
;; display file name to title
;;
(setq-default frame-title-format
              (list '((buffer-file-name " %f" (dired-directory dired-directory
                        (revert-buffer-function " %b" ("%b – Dir:  " default-directory)))))))
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
;; Disable scroll
;;
(put 'scroll-left 'disabled nil)

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
(add-to-list 'package-archives
             '(("melpa" . "http://melpa.org/packages/")))


(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        Load extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; ui
;;
(load "~/.emacscore/utils.el")
(load "~/.emacscore/text-utils.el")
(load "~/.emacscore/ui.el")
(load "~/.emacscore/keybindings.el")

;;
;; Development
;;
(load "~/.emacscore/dev/c.el")
(load "~/.emacscore/dev/erlang.el")
(load "~/.emacscore/dev/elixir.el")

;; Finish :)
(message "All done, %s%s" (user-login-name) ".")
(put 'dired-find-alternate-file 'disabled nil)
