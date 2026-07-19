;;; gpg.el --- Configuration for GPG  -*- lexical-binding: t -*-

;; to not forward us to another pinentry
(setq epg-pinentry-mode 'loopback)

;; Disable caching of passpharses
(setq epa-file-cache-passphrase-for-symmetric-encryption nil)
