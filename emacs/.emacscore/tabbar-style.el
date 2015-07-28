;;
;; Tabbar mode
;;
(add-to-list 'load-path "~/.emacs.d/tabbar")
(require 'tabbar)

;;
;; All tabs in one group
;;
(defun my-tabbar-buffer-groups ()
  (list (cond
         ((string-equal "#"
                        (substring (buffer-name) 1
                                   (length (buffer-name)))) "chat")

         ((string-equal ".h"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 2)
                                   (length (buffer-name)))) "system")
         ((string-equal ".c"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 2)
                                   (length (buffer-name)))) "system")
         ((string-equal ".asm"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 4)
                                   (length (buffer-name)))) "system")
         ((string-equal ".S"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 2)
                                   (length (buffer-name)))) "system")
         ((string-equal ".S"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 2)
                                   (length (buffer-name)))) "system")
         ((string-equal ".ld"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 3)
                                   (length (buffer-name)))) "system")
         ((string-equal "Makefile"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 8)
                                   (length (buffer-name)))) "system")
         ((string-equal ".mk"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 3)
                                   (length (buffer-name)))) "system")
         ((string-equal "Kbuild"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 6)
                                   (length (buffer-name)))) "system")
         ((string-equal "*t"
                        (substring (buffer-name) 0 2)) "terminal")

         ((string-equal ".el"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 3)
                                   (length (buffer-name)))) "elisp")
         ((string-equal ".emacs"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 6)
                                   (length (buffer-name)))) "elisp")
         ((string-equal ".erl"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 4)
                                   (length (buffer-name)))) "erlang")
         ((string-equal ".hrl"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 4)
                                   (length (buffer-name)))) "erlang")
         ((string-equal ".js"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 3)
                                   (length (buffer-name)))) "web")
         ((string-equal ".html"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 5)
                                   (length (buffer-name)))) "web")
         ((string-equal ".tpl.html"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 9)
                                   (length (buffer-name)))) "web")
         ((string-equal ".css"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 4)
                                   (length (buffer-name)))) "web")
         ((string-equal ".go"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 3)
                                   (length (buffer-name)))) "golang")
         ((string-equal ".sh"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 3)
                                   (length (buffer-name)))) "system")
         ((string-equal ".cpp"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 4)
                                   (length (buffer-name)))) "system")
         ((string-equal "*magit"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 6)
                                   (length (buffer-name)))) "git")
         ((string-equal ".org"
                        (substring (buffer-name)
                                   (- (length (buffer-name)) 4)
                                   (length (buffer-name)))) "org")

         ((string-equal "*"
                        (substring (buffer-name) 0 1))
          "emacs")

         ((eq major-mode 'dired-mode) "Dired")
         (t "user"))))

;; set up custom tabbar group
(setq tabbar-buffer-groups-function 'my-tabbar-buffer-groups)

;; tabbar background
(setq tabbar-background-color "#333333")

;;
;; selected tabbar
;;
(set-face-attribute
 'tabbar-selected nil
 :background "dim gray"
 :foreground "white"
 :family "Droid Sans Mono"
 :box '(:line-width 1 :color "gray60" :style nil))

;;
;; unselected tabbar
;;
(set-face-attribute
 'tabbar-unselected nil
 :background "#333333"
 :foreground "gray"
 :box '(:line-width 1 :color "#333333" :style nil))

;;
;; tabbar separator color
;;
(set-face-attribute
 'tabbar-separator nil
 :background "gray20"
 :height 0.6)

;;
;; tabbar buttons
;;
(set-face-attribute
 'tabbar-button nil
 :background "#333333"
 :box '(:line-width 1 :color "#333333" :style nil))

;;
;; Enable tabbar mode
;;
(tabbar-mode)
