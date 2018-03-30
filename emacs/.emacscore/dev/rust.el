;;; rust.el --- Rust routines for GNU Emacs  -*- lexical-binding: t -*-
;;
;; Author:  Alexander Kuleshov <kuleshovmail@gmail.com>
;; URL:     https://github.com/0xAX/med
;;
;; License: See LICENSE file.

(add-to-list 'load-path "~/.emacs.d/lisp/rust-mode.el")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(add-hook 'rust-mode-hook
  (lambda ()
    (setq indent-tabs-mode nil)
    (setq tab-width 4)
    (setq rust-indent-offset 4)))

(setq racer-cmd "~/.cargo/bin/racer")
(setq racer-rust-src-path "~/dev/rust/src")

(add-hook 'rust-mode-hook #'company-mode)
(setq company-tooltip-align-annotations t)
