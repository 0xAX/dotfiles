;;
;; Tabbar mode
;;
(add-to-list 'load-path "~/.emacs.d/tabbar")
(require 'tabbar)

;;
;; All tabs in one group
;;
(defun my-tabbar-buffer-groups ()
  (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs")
              ((eq major-mode 'dired-mode) "emacs")
              ((string-equal ".sh" (substring (buffer-name) (- (length (buffer-name)) 3) (length (buffer-name)))) "system")
              ((string-equal ".h" (substring (buffer-name) (- (length (buffer-name)) 2) (length (buffer-name)))) "system")
              ((string-equal ".c" (substring (buffer-name) (- (length (buffer-name)) 2) (length (buffer-name)))) "system")
              ((string-equal ".cc" (substring (buffer-name) (- (length (buffer-name)) 3) (length (buffer-name)))) "system")
              ((string-equal ".hh" (substring (buffer-name) (- (length (buffer-name)) 3) (length (buffer-name)))) "system")
              ((string-equal ".S" (substring (buffer-name) (- (length (buffer-name)) 2) (length (buffer-name)))) "system")
              ((string-equal ".s" (substring (buffer-name) (- (length (buffer-name)) 2) (length (buffer-name)))) "system")
              ((string-equal ".asm" (substring (buffer-name) (- (length (buffer-name)) 4) (length (buffer-name)))) "system")
              ((string-equal ".nasm" (substring (buffer-name) (- (length (buffer-name)) 5) (length (buffer-name)))) "system")
              ((string-equal ".ld" (substring (buffer-name) (- (length (buffer-name)) 3) (length (buffer-name)))) "system")
              ((string-equal "Makefile" (substring (buffer-name) (- (length (buffer-name)) 8) (length (buffer-name)))) "system")
              ((string-equal ".mk" (substring (buffer-name) (- (length (buffer-name)) 3) (length (buffer-name)))) "system")
              ((string-equal "Kbuild" (substring (buffer-name) (- (length (buffer-name)) 6) (length (buffer-name)))) "system")
              ((string-equal ".inc" (substring (buffer-name) (- (length (buffer-name)) 4) (length (buffer-name)))) "system")
              ((string-equal "KConfig" (substring (buffer-name) (- (length (buffer-name)) 7) (length (buffer-name)))) "system")
              ((string-equal "Kconfig" (substring (buffer-name) (- (length (buffer-name)) 7) (length (buffer-name)))) "system")
              ((string-equal "*t" (substring (buffer-name) (- (length (buffer-name)) 2) (length (buffer-name)))) "terminal")
              ((string-equal ".el" (substring (buffer-name) (- (length (buffer-name)) 3) (length (buffer-name)))) "lisp")
              ((string-equal ".emacs" (substring (buffer-name) (- (length (buffer-name)) 6) (length (buffer-name)))) "lisp")
              ((string-equal ".lisp" (substring (buffer-name) (- (length (buffer-name)) 5) (length (buffer-name)))) "lisp")
              ((string-equal ".erl" (substring (buffer-name) (- (length (buffer-name)) 4) (length (buffer-name)))) "erlang")
              ((string-equal ".ex" (substring (buffer-name) (- (length (buffer-name)) 3) (length (buffer-name)))) "erlang")
              ((string-equal ".exs" (substring (buffer-name) (- (length (buffer-name)) 4) (length (buffer-name)))) "erlang")
              ((string-equal ".hrl" (substring (buffer-name) (- (length (buffer-name)) 4) (length (buffer-name)))) "erlang")
              ((string-equal ".json" (substring (buffer-name) (- (length (buffer-name)) 5) (length (buffer-name)))) "web")
              ((string-equal ".js" (substring (buffer-name) (- (length (buffer-name)) 3) (length (buffer-name)))) "web")
              ((string-equal ".html" (substring (buffer-name) (- (length (buffer-name)) 5) (length (buffer-name)))) "web")
              ((string-equal ".css" (substring (buffer-name) (- (length (buffer-name)) 4) (length (buffer-name)))) "web")
              ((string-equal ".tpl.html" (substring (buffer-name) (- (length (buffer-name)) 9) (length (buffer-name)))) "web")
              ((string-equal ".rb" (substring (buffer-name) (- (length (buffer-name)) 3) (length (buffer-name)))) "web")
              ((string-equal ".php" (substring (buffer-name) (- (length (buffer-name)) 5) (length (buffer-name)))) "web")
              ((string-equal ".go" (substring (buffer-name) (- (length (buffer-name)) 3) (length (buffer-name)))) "golang")
              ((string-equal ".md" (substring (buffer-name) (- (length (buffer-name)) 3) (length (buffer-name)))) "doc")
              ((string-equal ".rst" (substring (buffer-name) (- (length (buffer-name)) 4) (length (buffer-name)))) "doc")
              ((string-equal ".org" (substring (buffer-name) (- (length (buffer-name)) 4) (length (buffer-name)))) "doc")
              (t "user"))))

;; set up custom tabbar group
(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

;; tabbar background
(setq tabbar-background-color "#263238")

;;
;; selected tabbar
;;
(set-face-attribute
 'tabbar-selected nil
 :background "#263238"
 :foreground "#b0bec5"
 :family "Droid Sans Mono"
 :box '(:line-width 1 :color "#263238" :style nil))

;;
;; unselected tabbar
;;
(set-face-attribute
 'tabbar-unselected nil
 :background "#263238"
 :foreground "#8a8a8a"
 :box '(:line-width 1 :color "#263238" :style nil))

;;
;; tabbar separator color
;;
(set-face-attribute
 'tabbar-separator nil
 :background "#263238"
 :height 0.5)

;;
;; Enable tabbar mode
;;
(tabbar-mode)
