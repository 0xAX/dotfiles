;;; magit.el --- My configuration for magit -*- lexical-binding: t -*-
;;
;; Enable additional suffixes
;;
;; For more information see:
;;  * https://magit.vc/manual/transient/Enabling-and-Disabling-Suffixes.html
;;  * https://emacsair.me/2019/02/14/transient-0.1/

(require 'magit)

;; Enable more suffixes
(setq transient-default-level 7)

;; Increase the number of the commits shown on the main panel
(setq magit-log-section-commit-count 50)

;; unbind magit-section keybindings as they are used to navigate
;; in my i3 setup
(with-eval-after-load "magit"
  (define-key magit-section-mode-map (kbd "M-1") nil)
  (define-key magit-section-mode-map (kbd "M-2") nil)
  (define-key magit-section-mode-map (kbd "M-3") nil)
  (define-key magit-section-mode-map (kbd "M-4") nil)
  (define-key magit-section-mode-map [C-tab] nil)
  (define-key magit-section-mode-map [M-tab] nil))
