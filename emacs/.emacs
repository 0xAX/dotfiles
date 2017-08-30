;; .emacs
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
;; As we started switch to second workspace
;;
(load "~/.emacscore/desktop/i3.el")
(i3-switch-workspace "2")

;;
;; exit from i3 passthrough mode on exit
;;
(add-hook 'kill-emacs-hook
          (lambda ()
            (shell-command-to-string "i3-msg mode default")))

;;
;; do not save sessions
;;
(desktop-save-mode 0)

;;
;; Delete text in selection mode when typing
;;
(delete-selection-mode 1)

;;
;; display file name in title
;;
(setq-default frame-title-format
              (list '((buffer-file-name " %f" (dired-directory dired-directory
                        (revert-buffer-function " %b" ("%b – Dir:  " default-directory)))))))

;;
;; Current locale
;;
(prefer-coding-system        'utf-8)
(set-terminal-coding-system  'utf-8)
(set-keyboard-coding-system  'utf-8)
(set-selection-coding-system 'utf-8)
(setq locale-coding-system   'utf-8)
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
;; yes-or-no-p to y-n
;;
(defalias 'yes-or-no-p 'y-or-n-p)

;;
;; Disable system beep
;;
(setq visible-bell nil) ;; The default
(setq ring-bell-function 'ignore)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        Package manager
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        Load extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; ui
;;
(load "~/.emacscore/utils.el")
(load "~/.emacscore/work-utils.el")
(load "~/.emacscore/text-utils.el")
(load "~/.emacscore/ui.el")
(load "~/.emacscore/keybindings.el")
(load "~/.emacscore/org/orgmode.el")

;;
;; Development
;;
(load "~/.emacscore/dev/erlang.el")
(load "~/.emacscore/dev/elixir.el")
(load "~/.emacscore/dev/c.el")
(load "~/.emacscore/dev/golang.el")
(load "~/.emacscore/dev/rust.el")
(load "~/.emacscore/vcs/magit.el")
(load "~/.emacscore/term.el")

;;
;; load snippets
;;
(add-to-list 'load-path "~/.emacs.d/yasnippet")
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacscore/snippets"))
(yas-global-mode 1)

;; Finish :)
(message "All done, %s%s" (user-login-name) ".")
(put 'dired-find-alternate-file 'disabled nil)
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
 '(org-directory "~/todo" t)
 '(package-selected-packages (quote (bison-mode racer))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :background "#263238" :foreground "CadetBlue1" :box nil :weight bold :height 1.0))))
 '(org-level-2 ((t (:inherit outline-2 :background "#263238" :foreground "CadetBlue2" :box nil :height 1.0))))
 '(org-level-3 ((t (:inherit outline-3 :background "#263238" :foreground "CadetBlue3" :height 0.9))))
 '(org-level-4 ((t (:inherit outline-4 :background "#263238" :foreground "#00bfff" :height 0.8))))
 '(org-level-5 ((t (:inherit outline-5 :background "#263238" :foreground "DarkOrange1"))))
 '(org-level-6 ((t (:inherit outline-6 :background "#263238" :foreground "DarkOrange2"))))
 '(org-level-7 ((t (:inherit outline-7 :background "#263238" :foreground "DarkOrange3"))))
 '(org-level-8 ((t (:inherit outline-8 :background "#263238" :foreground "gold2"))))
 '(org-todo ((t (:background "#263238" :foreground "orange red" :weight bold))))
 '(show-paren-match ((t (:background "#116599" :foreground "white")))))
