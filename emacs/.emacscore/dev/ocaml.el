(add-to-list 'load-path "~/.emacs.d/tuareg")

(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)
(autoload 'tuareg-imenu-set-imenu "tuareg-imenu"
  "Configuration of imenu for tuareg" t)

(add-hook 'tuareg-mode-hook
          (lambda()
            (when (functionp 'prettify-symbols-mode)
              (prettify-symbols-mode))))

(setq auto-mode-alist 
      (append '(("\\.ml[ily]?$" . tuareg-mode)
                ("\\.topml$" . tuareg-mode))
              auto-mode-alist))
