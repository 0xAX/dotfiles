;;; ocaml.el --- C/C++ for GNU Emacs  -*- lexical-binding: t -*-

;; make some non-standard symbols better
(prettify-symbols-mode t)
(setq tuareg-prettify-symbols-full t)

;; prettify ocaml-mode
(add-hook 'tuareg-mode-hook #'(lambda() (setq mode-name "🐫")))

;; Enable ligatures
(add-hook 'tuareg-mode-hook
          (lambda()
            (when (functionp 'prettify-symbols-mode)
              (prettify-symbols-mode))))

;; enable auto-complete if ocamlmerlin is installed
(when (executable-find "ocamlmerlin")
  (merlin-mode))
