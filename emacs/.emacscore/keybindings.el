;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;        Key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The F10, F10, F11, F12 are free
(global-set-key [f1]  'dired-other-window)
(global-set-key [f2]  'sudo-find-file)
(global-set-key [f3]  'reload-file)
(global-set-key [f4]  'eval-buffer)
(global-set-key [f5]  'open-terminal)
(global-set-key [f6]  'rename-file-and-buffer)
(global-set-key [f7]  'delete-this-buffer-and-file)
(global-set-key [f8]  'rgrep)
(global-set-key [f9]  'ibuffer)
(global-set-key [f10]  'magit-status)
(global-set-key [f12] 'load-additional-dev-modes)

;;
;; Unset some standard key bindings
;;
(global-unset-key "\C-f")
(global-unset-key "\C-l")
(global-unset-key "\C-b")

;;
;; external applications
;;
(global-set-key (kbd "C-x f")   'ftp)
(global-set-key (kbd "C-x t")   'telnet)
(global-set-key (kbd "C-x g")   'gdb)
(global-set-key (kbd "C-x RET") 'calc)
(global-set-key (kbd "C-x c")   'calendar)
(global-set-key (kbd "C-x b")   'browse-url)
(global-set-key (kbd "C-x p")   'ping)
(global-set-key (kbd "C-x l")   'list-colors-display)

;;
;; Text manipulation
;;
(global-set-key "\C-a" 'mark-page)
(global-set-key "\M-u" 'untabify)
(global-set-key "\M-r" 'replace-all)
(global-set-key "\M-s" 'mark-curr-line)
(global-set-key "\M-k" 'kill-full-line)
(global-set-key "\C-g" 'goto-line)
(global-set-key "\C-s" 'isearch-forward)
(global-set-key (kbd "TAB") 'tab-to-tab-stop)

;;
;; buffers manipulations
;;
(global-set-key "\C-k" 'kill-this-buffer)
(global-set-key "\C-f" 'file-name)
(global-set-key "\M-w" 'save-buffers-kill-emacs)
(global-set-key "\M-d" 'delete-this-buffer-and-file)
(global-set-key "\C-b" 'insert-bash-she-bang)

(global-set-key (kbd "M-<left>")  'beginning-of-line)
(global-set-key (kbd "M-<right>") 'end-of-line)
(global-set-key (kbd "M-<up>")  'beginning-of-buffer)
(global-set-key (kbd "M-<down>") 'end-of-buffer)
(global-set-key (kbd "C-c c") 'emacs-config)

;;
;; tabbar keybindings
;;
(global-set-key [C-left]  'tabbar-backward-tab)
(global-set-key [C-right] 'tabbar-forward-tab)
(global-set-key [C-tab]   'tabbar-forward-group)

;;
;; Undo/Redo
;;
(global-unset-key "\C-r")
(global-set-key "\C-z" 'undo)
(global-set-key "\C-r" 'redo)

;;
;; ui
;;
(global-set-key [M-f12] 'toggle-mode-line)

;;
;; Help
;;
(global-set-key (kbd "C-h") 'discover-my-major)

;;
;; Window size manipulation
;;
(global-set-key (kbd "C-S-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "<C-down>") 'shrink-window)
(global-set-key (kbd "<C-up>") 'enlarge-window)

;;
;; package manager
;;
(global-unset-key "\C-p")
(global-set-key (kbd "C-p l") 'package-list-packages)
(global-set-key (kbd "C-p i") 'package-install)

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

;; org-mode keybindings
(global-unset-key "\C-t")
(global-set-key (kbd "C-t t") 'org-todo-list)
