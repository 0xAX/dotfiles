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
(add-to-list 'load-path "~/dev/git2el")

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
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

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
(load "~/.emacscore/dev/erlang.el")
(load "~/.emacscore/dev/elixir.el")
(load "~/.emacscore/vcs/magit.el")

(add-to-list 'load-path "~/.emacs.d/emacs-neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ido-enable-last-directory-history nil)
 '(ido-max-work-directory-list 0)
 '(ido-max-work-file-list 0)
 '(ido-record-commands nil)
 '(indent-tabs-mode nil)
 '(package-selected-packages
   (quote
    (magit-annex magit-filenotify magit-find-file magit-gerrit magit-gh-pulls magit-gitflow magit-p4 magit-popup magit-rockstar magit-stgit magit-svn magit-topgit magit debbugs seq racer ht gotest go-scratch go-rename go-guru go-eldoc go-direx go-autocomplete company-go company-emoji company-c-headers))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(show-paren-match ((t (:background "#116599" :foreground "white")))))

;;
;; highlight for mutt messages
;;
(add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

;;
;; Load *.c later to make working indentation
;;
(load "~/.emacscore/dev/c.el")

;; Finish :)
(message "All done, %s%s" (user-login-name) ".")
(put 'dired-find-alternate-file 'disabled nil)
