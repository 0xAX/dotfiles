;;; ido.el --- My configuration for ido  -*- lexical-binding: t -*-

;; enable ido mode
(require 'ido)
(setq ido-save-directory-list-file nil)
(setq ido-enable-flex-matching t)
(ido-mode 1)
(ido-everywhere 1)

;; do not record names of opened files
(setq ido-max-work-directory-list 0)
;; allow to show all files
(setq ido-max-work-file-list 0)
;; do not record ido commands
(setq ido-record-commands nil)
;; do not remember latest selected directory name
(setq ido-enable-last-directory-history nil)
