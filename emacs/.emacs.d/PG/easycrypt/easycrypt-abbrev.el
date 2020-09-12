;;; easycrypt-abbrev.el --- Abbrev table and menus for EasyCrypt mode

;; --------------------------------------------------------------------
;; Copyright (c) - 2012--2016 - IMDEA Software Institute
;; Copyright (c) - 2012--2016 - Inria
;;
;; Distributed under the terms of the GPL-v3 license
;; --------------------------------------------------------------------

;;; Commentary:
;; 

(require 'proof)
(require 'easycrypt-syntax)

;;; Code:

(defpgdefault menu-entries
  '(
    ["Use Three Panes" proof-three-window-toggle
      :style    toggle
      :active   (not proof-multiple-frames-enable)
      :selected proof-three-window-enable
      :help     "Use three panes"]

    ["Weak-check mode" easycrypt-proof-weak-mode-toggle
     :style    toggle
     :selected easycrypt-proof-weak-mode
     :help     "Toggles EasyCrypt check mode."]
))

(provide 'easycrypt-abbrev)

;;; easycrypt-abbrev.el ends here
