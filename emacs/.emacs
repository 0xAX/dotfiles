;;; .emacs --- My init file for GNU Emacs  -*- lexical-binding: t -*-

;; Set directory for dependencies and primary configuration
(setq user-emacs-directory "~/.emacs.d")
;; Set directory with secondary configuration
(setq user-emacs-configuration-directory "~/.emacscore")

;; If we are using i3wm, load related configuration.
(let*
    ((i3-socket (shell-command-to-string "i3 --get-socketpath"))
     (i3 (file-exists-p (replace-regexp-in-string "\n$" "" i3-socket))))
  (if i3
      (progn
        (load "~/.emacscore/desktop/i3.el")
        (setq *i3* "true"))
    (setq *i3* "false")))

;; do not save sessions
(desktop-save-mode 0)
;; Delete text in selection mode when typing
(delete-selection-mode 1)

;; Set character sets
(prefer-coding-system        'utf-8)
;; Set coding system of terminal output
(set-terminal-coding-system  'utf-8)
;; Set codign system for keyboard input
(set-keyboard-coding-system  'utf-8)
;; Set coding system to communicate with other X systems
(set-selection-coding-system 'utf-8)
;; Set coding system to use with system messages
(setq locale-coding-system   'utf-8)
;; Set language environment
(setq current-language-environment "UTF-8")

;; Prevent creation of backup files
(setq make-backup-files         nil)
(setq auto-save-list-file-name  nil)
(setq auto-save-default         nil)
(setq create-lockfiles          nil)

;; Load the dependencies if something is missed
(load "~/.emacscore/dependencies.el")
(update-packages)

;; Load the small libraries first
(load "~/.emacscore/lisp-utils.el")
(load "~/.emacscore/lisp/org-bullets.el")
(load "~/.emacscore/lisp/cmake.el")
(load "~/.emacscore/lisp/rainbow-mode-1.0.5.el")
(load "~/.emacscore/lisp/org-bullets.el")
(load "~/.emacscore/lisp/org-fragtog.el")
(load "~/.emacscore/lisp/epl.el")
(load "~/.emacscore/lisp/pkg-info.el")

;; Emacs UI and utils
(load "~/.emacscore/system.el")
(load "~/.emacscore/file-utils.el")
(load "~/.emacscore/text-utils.el")
(load "~/.emacscore/keybindings.el")
(load "~/.emacscore/org/orgmode.el")
(load "~/.emacscore/ido.el")
(load "~/.emacscore/dired.el")
(load "~/.emacscore/gpg.el")
(load "~/.emacscore/ui.el")
(load "~/.emacscore/jit-and-gc.el")
(load "~/.emacscore/vcs/magit.el")

;; Development
(load "~/.emacscore/dev/lsp-mode.el")
(load "~/.emacscore/dev/sed.el")
(load "~/.emacscore/dev/shell.el")
(load "~/.emacscore/dev/elisp.el")
(load "~/.emacscore/dev/erlang.el")
(load "~/.emacscore/dev/elixir.el")
(load "~/.emacscore/dev/company.el")
(load "~/.emacscore/dev/c.el")
(load "~/.emacscore/dev/common-lisp.el")
;;(load "~/.emacscore/dev/zig.el")

;; Load miscellaneous things
(load "~/.emacscore/term.el")
(load "~/.emacscore/snippets.el")
(load "~/.emacscore/build/make.el")
(load "~/.emacscore/markups.el")
(load "~/.emacscore/dotfiles.el")

;; do not use tabs for indentation at all
(setq-default indent-tabs-mode nil)

;; Hide *async-native-compile* buffer
(setq warning-minimum-level :error)

;; custom-set-faces was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-level-1 ((t (:inherit outline-1 :height 0.7))))
 '(org-level-2 ((t (:inherit outline-2 :height 0.8))))
 '(org-level-3 ((t (:inherit outline-3 :height 0.799))))
 '(org-level-4 ((t (:inherit outline-4 :height 0.777))))
 '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
 '(tabbar-default ((t (:background "#fdf6e3" :foreground "#eee8d5" :font "Fira Code-12"))))
 '(tabbar-modified ((t (:background "#fdf6e3" :foreground "#d33682"))))
 '(tabbar-selected ((t (:background "#fdf6e3" :foreground "#839496"))))
 '(tabbar-separator ((t (:background "#fdf6e3" :foreground "#fdf6e3"))))
 '(tabbar-unselected ((t (:background "#fdf6e3" :foreground "#93a1a1")))))

;; finally loaded everything
(message "All done, %s%s" (user-login-name) ".")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil)
 '(package-vc-selected-packages
   '((with-editor :url "https://github.com/magit/with-editor.git"
       :lisp-dir "lisp")
     (emacs-libvterm :vc-backend Git :url
                     "https://github.com/akermu/emacs-libvterm.git")
     (yasnippet-snippets :vc-backend Git :url
                         "https://github.com/AndreaCrotti/yasnippet-snippets")
     (yasnippet :vc-backend Git :url
                "https://github.com/joaotavora/yasnippet")
     (yaml-mode :vc-backend Git :url
                "https://github.com/yoshiki/yaml-mode")
     (tabbar :vc-backend Git :url
             "https://github.com/dholm/tabbar.git")
     (swiper :vc-backend Git :url
             "https://github.com/abo-abo/swiper.git")
     (slime-company :vc-backend Git :url
                    "https://github.com/anwyn/slime-company.git")
     (slime :vc-backend Git :url "https://github.com/slime/slime.git")
     (solarized-emacs :vc-backend Git :url
                      "https://github.com/bbatsov/solarized-emacs.git")
     (rustic :vc-backend Git :url
             "https://github.com/brotzeit/rustic.git")
     (rust-mode :vc-backend Git :url
                "https://github.com/rust-lang/rust-mode.git")
     (markdown-mode :vc-backend Git :url
                    "https://github.com/jrblevin/markdown-mode.git")
     (magit :url "https://github.com/magit/magit.git" :lisp-dir "lisp")
     (lsp-ui :vc-backend Git :url
             "https://github.com/emacs-lsp/lsp-ui.git")
     (lsp-treemacs :vc-backend Git :url
                   "https://github.com/emacs-lsp/lsp-treemacs.git")
     (lsp-mode :vc-backend Git :url
               "https://github.com/emacs-lsp/lsp-mode.git")
     (ivy-posframe :vc-backend Git :url
                   "https://github.com/tumashu/ivy-posframe.git")
     (go-mode.el :vc-backend Git :url
                 "https://github.com/dominikh/go-mode.el.git")
     (flycheck :vc-backend Git :url
               "https://github.com/flycheck/flycheck.git")
     (f.el :vc-backend Git :url "https://github.com/rejeep/f.el.git")
     (erlang-mode :url "https://github.com/erlang/otp" :lisp-dir
                  "lib/tools/emacs")
     (emacs-elixir :vc-backend Git :url
                   "https://github.com/elixir-editors/emacs-elixir.git")
     (dash.el :vc-backend Git :url
              "https://github.com/magnars/dash.el.git")
     (company-c-headers :vc-backend Git :url
                        "https://github.com/randomphrase/company-c-headers.git")
     (company-mode :vc-backend Git :url
                   "https://github.com/company-mode/company-mode.git")
     (bnf-mode :vc-backend Git :url
               "https://github.com/sergeyklay/bnf-mode.git")
     (bison-mode :vc-backend Git :url
                 "https://github.com/Wilfred/bison-mode.git"))))
