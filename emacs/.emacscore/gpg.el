;;; gpg.el --- Configuration for GPG  -*- lexical-binding: t -*-

;; Use gpg2 always
(setq epg-gpg-program "gpg2")

;; to not forward us to another pinentry
(setq epg-pinentry-mode 'loopback)

;; Disable caching of passpharses
(setq epa-file-cache-passphrase-for-symmetric-encryption nil)

;; Set encoding for gpg files
(add-hook 'epa-file-mode-hook
          (lambda ()
            (set-buffer-file-coding-system 'utf-8)))

;; Hint Emacs to treat .gpg buffers as UTF-8 after decryption.
(add-to-list 'file-coding-system-alist '("\\.gpg\\'" . utf-8))
(add-to-list 'file-coding-system-alist '("\\.org\\'" . utf-8))
