(load "~/.emacs.d/tuareg/tuareg-site-file.el")

(setq auto-mode-alist
        (append '(("\\.ml[ily]?$" . tuareg-mode)
	          ("\\.topml$" . tuareg-mode))
				auto-mode-alist))
