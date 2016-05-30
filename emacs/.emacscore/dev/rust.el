(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))

(add-hook 'rust-mode-hook
  (lambda ()
    (setq indent-tabs-mode nil)
    (setq tab-width 4)
    (setq rust-indent-offset 4)))

(setq racer-cmd "~/.cargo/bin/racer")
(setq racer-rust-src-path "~/dev/rust/src")

(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'rust-mode-hook #'company-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)

(setq company-tooltip-align-annotations t)
