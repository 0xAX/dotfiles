;;; dired.el --- My configuration for dired  -*- lexical-binding: t -*-

;;
;; Dired configuration
;;
(require 'dired-x)
(require 'dired)
(setq dired-listing-switches "-lapq --author --file-type --group-directories-first
                              -h --human-readable --hide='*.o' --hide='*.d' ")

;; open file in a dired
(put 'dired-find-alternate-file 'disabled nil)
