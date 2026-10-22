;;; rocq.el --- Rocq (Coq) for GNU Emacs  -*- lexical-binding: t -*-

;; Proof General still calls Rocq "Coq" everywhere: `.v' files open in
;; `coq-mode' and the settings are `coq-*' and `proof-*'.  It finds the
;; `rocq' executable on PATH by itself and runs it through the Rocq 9 CLI
;; (`rocq top', `rocq c', `rocq dep'), so nothing needs pointing at it.

;; No splash screen when the first .v file is opened
(setq proof-splash-enable nil)
