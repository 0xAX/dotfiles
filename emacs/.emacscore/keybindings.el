;;; keybindings.el --- Global keybindings -*- lexical-binding: t -*-

;; F(n) keybindings
(global-set-key [f1]  'dired-other-window)
(global-set-key [f2]  'sudo-find-file)
(global-set-key [f3]  'reload-file)
(global-set-key [f4]  'eval-buffer)
(global-set-key [f5]  'open-terminal)
(global-set-key [f6]  'rename-file-and-buffer)
(global-set-key [f7]  'delete-this-buffer-and-file)
(global-set-key [f8]  'rgrep)
(global-set-key [f9]  'ibuffer)
(global-set-key [f10] 'magit-status)
(global-set-key [f11] 'set-debug-emacs)

;; Unset some standard key bindings
(global-unset-key "\C-f")
(global-unset-key "\C-l")
(global-unset-key "\C-b")
(global-unset-key "\C-n")
(global-unset-key "\C-d")
(global-unset-key "\C-w")

;; External applications
(global-set-key (kbd "C-x f")   'ftp)
(global-set-key (kbd "C-x t")   'todo)
(global-set-key (kbd "C-x g")   'gdb)
(global-set-key (kbd "C-x RET") 'calc)
(global-set-key (kbd "C-x y")   'calendar)
(global-set-key (kbd "C-x b")   'browse-url)
(global-set-key (kbd "C-x p")   'ping)
(global-set-key (kbd "C-x l")   'list-colors-display)
(global-set-key (kbd "C-x m")   'mu4e)
(global-set-key (kbd "C-x e")   'eshell)

;; Text manipulation and cursor manipulation
(global-set-key "\C-a" 'mark-page)
(global-set-key "\M-u" 'untabify)
(global-set-key "\M-r" 'replace-all)
(global-set-key "\M-s" 'mark-curr-line)
(global-set-key "\M-k" 'kill-full-line)
(global-set-key "\C-g" 'goto-line)
(global-set-key "\C-s" 'isearch-forward)
(global-set-key "\C-b" 'isearch-backward)
(global-set-key (kbd "C-w b") 'backward-word)
(global-set-key (kbd "C-w d") 'kill-word)
(global-set-key (kbd "C-w f") 'forward-word)
(global-set-key (kbd "TAB") 'tab-to-tab-stop)

;; buffers manipulations
(global-set-key "\C-k" (lambda () (interactive) (kill-buffer (current-buffer))))
(global-set-key "\M-w" 'save-buffers-kill-emacs)
(global-set-key "\M-d" 'delete-this-buffer-and-file)
(global-set-key (kbd "M-<left>")  'beginning-of-line)
(global-set-key (kbd "M-<right>") 'end-of-line)
(global-set-key (kbd "M-<up>")  'beginning-of-buffer)
(global-set-key (kbd "M-<down>") 'end-of-buffer)
(global-set-key (kbd "C-c c") 'emacs-config)
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; help
(global-set-key (kbd "C-h m") 'describe-mode)
(global-set-key (kbd "C-h k") 'describe-key)
(global-set-key (kbd "C-h b") 'describe-bindings)
(global-set-key (kbd "C-h f") 'describe-function)
(global-set-key (kbd "C-h v") 'describe-variable)
(global-set-key (kbd "C-h s") 'describe-symbol)

;; Encoding
(global-set-key (kbd "C-c r e") #'revert-buffer-utf8)

;; tabbar keybindings
(if (equal tab-mode 'tabbar)
    (progn
      (global-set-key [C-left]  'tabbar-backward-tab)
      (global-set-key [C-right] 'tabbar-forward-tab)
      (global-set-key [C-tab]   'tabbar-forward-group))
    (progn
      (global-set-key [C-left]  'centaur-tabs-backward)
      (global-set-key [C-right] 'centaur-tabs-forward)
      (global-set-key [C-tab] (lambda () (interactive) (centaur-tabs-forward-group)))))

;; files manipulation
(global-set-key "\C-n" 'new-file)

;; Window size manipulation
(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "<C-down>") 'shrink-window)
(global-set-key (kbd "<C-up>") 'enlarge-window)

;; package manager
(global-unset-key "\C-p")
(global-set-key (kbd "C-p l") 'package-list-packages)

;; increase/decrease fonts
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; keybinding to switch other workspaces
(global-set-key "\M-1" 'go-to-workspace-1)
(global-set-key "\M-2" 'go-to-workspace-2)
(global-set-key "\M-3" 'go-to-workspace-3)
(global-set-key "\M-4" 'go-to-workspace-4)
(global-set-key "\M-5" 'go-to-workspace-5)
(global-set-key "\M-6" 'go-to-workspace-6)
(global-set-key "\M-7" 'go-to-workspace-7)
(global-set-key "\M-8" 'go-to-workspace-8)
(global-set-key "\M-9" 'go-to-workspace-9)

;; indentation
(global-set-key (kbd "RET") 'newline-and-indent)

;; i3 keybindings
(when (string= *i3* "true")
  (global-set-key (kbd "<M-tab>") 'switch-to-next-i3-workspace))

;; hyprland keybindings
(when (string= *hyprland* "true")
  (global-set-key (kbd "<M-tab>") 'switch-to-next-hyprland-workspace))

;; dotfiles repo
(global-set-key (kbd "C-d C-u") 'update-dotfiles-repo)
(global-set-key (kbd "C-d C-s") 'update-system-dotfiles)

;; magit keybindings
(global-set-key (kbd "C-x C-l") 'magit-log-buffer-file)

;; treemacs keybindings
(global-set-key "\C-t" 'treemacs)

;;
;; mode local keybindings
;;

;; Special cases for `C-+` keybinding for dired
(add-hook 'dired-mode-hook
  (lambda ()
   (local-set-key (kbd "C-+") 'dired-create-directory)))

;; keybindings for rust-mode
(add-hook 'rust-mode-hook
  (lambda ()
    (local-set-key (kbd "C-c C-c") 'rust-compile)
    (local-set-key (kbd "C-c C-k") 'rust-check)
    (local-set-key (kbd "C-c C-t") 'rust-test)
    (local-set-key (kbd "C-c C-l") 'rust-run-clippy)))
