;; .emacs
;;
;;
;;        DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;;                Version 2, December 2004
;;
;; Everyone is permitted to copy and distribute verbatim or modified
;; copies of this license document, and changing it is allowed as long
;; as the name is changed.
;;
;;         DO WHAT THE FUCK YOU WANT TO PUBLIC LICENSE
;; TERMS AND CONDITIONS FOR COPYING, DISTRIBUTION AND MODIFICATION
;;
;;  0. You just DO WHAT THE FUCK YOU WANT TO.
;;
;;
;;
;;                                                            0xAX :)
;;

;;
;; mark current line
;;
(defun mark-curr-line ()
  "Set mark for current line"
  (beginning-of-line)
  (cua-set-mark)
  (end-of-line))

;;
;; replace all
;;
(defun replace-all (find replace)
  "Find&Replace all"
  (interactive
   (list
        (read-string "Find: ")
        (read-string "Replace with: ")))
  (setq cursor-position (point))
  (beginning-of-buffer)
  (replace-string find replace)
  (goto-char cursor-position)
  (message "done"))

;;
;; kill full line
;;
(defun kill-full-line()
  "Kill line from the beginnig of line"
  (beginning-of-line)
  (kill-line)
  (delete-backward-char 1))
