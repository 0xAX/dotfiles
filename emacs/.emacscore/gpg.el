;;; gpg.el --- Configuration for GPG  -*- lexical-binding: t -*-

;; to not forward us to another pinentry
(setq epg-pinentry-mode 'loopback)

;; Disable caching of passpharses.
;; `org-reading-call' in org/orgmode.el rebinds this to t for the duration of a
;; reading-notes tags search, where the alternative is one prompt per encrypted
;; file; the global default stays off.
(setq epa-file-cache-passphrase-for-symmetric-encryption nil)
