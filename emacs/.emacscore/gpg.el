;;; gpg.el --- Configuration for GPG  -*- lexical-binding: t -*-

;; Use gpg2 always
(setq epg-gpg-program "gpg2")

;; to not forward us to another pinentry
(setq epg-pinentry-mode 'loopback)

;; Disable caching of passpharses
(setq epa-file-cache-passphrase-for-symmetric-encryption nil)
