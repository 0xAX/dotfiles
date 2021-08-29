(require 'lsp-mode)

;; install dependencies for lsp-mode

(when (file-exists-p "/etc/fedora-release")
  (when (string= (shell-command-to-string "command -v clangd") "")
    (sudo-shell-command "dnf install clang-tools-extra"))
  (when (string= (shell-command-to-string "command -v bear") "")
    (sudo-shell-command "dnf install bear")))

;; Setup keybindings for lsp-mode
(setq lsp-keymap-prefix "C-c C-l")

;; configure emacs-lisp gc and other runtime things for lsp-mode
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)

;; disable breadcrumb
(setq lsp-headerline-breadcrumb-enable nil)

;; Enable highlighting mode for symbols
(setq lsp-enable-symbol-highlighting t)

;; set font fo lsp-mod-ui-doc
(add-hook 'lsp-ui-doc-frame-hook
          (lambda (frame _w)
            (set-face-attribute 'default frame :font "Firacode" :height 150)))
