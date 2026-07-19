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

;; Don't auto-display the magit process buffer
(setq magit-process-popup-time -1)

;; unbind magit-section keybindings as they are used to navigate
;; in my i3 setup
(with-eval-after-load "magit"
  (define-key magit-section-mode-map (kbd "M-1") nil)
  (define-key magit-section-mode-map (kbd "M-2") nil)
  (define-key magit-section-mode-map (kbd "M-3") nil)
  (define-key magit-section-mode-map (kbd "M-4") nil)
  (define-key magit-section-mode-map [C-tab] nil)
  (define-key magit-section-mode-map [M-tab] nil))

;; Load the git-gutter and fringe
(require 'git-gutter)
;; (git-gutter--turn-on)
(require 'git-gutter-fringe)

;; Enable it in the text mode and as a result in all modes.
;; Skip epa-encrypted files: git stores the encrypted blob, so the
;; diff is meaningless, and live-update would write the decrypted
;; plaintext to /tmp.
(add-hook 'text-mode-hook
          (lambda ()
            (unless (and buffer-file-name
                         (string-match-p epa-file-name-regexp
                                         buffer-file-name))
              (git-gutter-mode 1))))

;; Configure git-gutter
(setq
 git-gutter:start-revision "HEAD"
 git-gutter:update-interval 0.05)

;; Set both the left and right fringe widths to 8 pixels.
(set-fringe-mode '(8 . 8))

;; Customize the fringe indicators.
(fringe-helper-define 'git-gutter-fr:added '(center repeated)
  "XXX....."
  "XXX....."
  "XXX....."
  "XXX.....")
(fringe-helper-define 'git-gutter-fr:modified '(center repeated)
  "XXX....."
  "XXX....."
  "XXX....."
  "XXX.....")
(fringe-helper-define 'git-gutter-fr:deleted 'bottom
  "X......."
  "XX......"
  "XXX....."
  "XXXX...."
  "XXXXX..."
  "XXXXXX.."
  "XXXXXXX."
  "XXXXXXXX"
  "XXXXXXX."
  "XXXXXX.."
  "XXXXX..."
  "XXXX...."
  "XXX....."
  "XX......"
  "X.......")
