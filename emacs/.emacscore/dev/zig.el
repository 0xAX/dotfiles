;;; zig.el --- Zig configuration for GNU Emacs  -*- lexical-binding: t -*-
(add-hook 'zig-mode-hook #'lsp)

;; clear value of the lsp-zig-zls-executable
(makunbound 'lsp-zig-zls-executable)

;; Provide path to the zls executable
(when (and
       (file-executable-p "/home/alex/disk/dev/zls/zig-out/bin/zls")
       (file-executable-p "/home/alex/disk/dev/zig/zig"))
  (setq lsp-zig-zls-executable "/home/alex/disk/dev/zls/zig-out/bin/zls"))

;; Provide alternative path for zls executable
(when (not (boundp 'lsp-zig-zls-executable))
  (when (and
       (file-executable-p "/home/alex/dev/zls/zig-out/bin/zls")
       (file-executable-p "/home/alex//dev/zig/zig"))
  (setq lsp-zig-zls-executable "/home/alex/dev/zls/zig-out/bin/zls")))
