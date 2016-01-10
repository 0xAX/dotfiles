;;
;; bash is a standard shell
;;
(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "bash")
(setenv "SHELL" shell-file-name)

(defadvice ansi-term (after advise-ansi-term-coding-system activate)
  (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))

;; close the terminal buffer automatically on exit
(defadvice term-sentinel (around my-advice-term-sentinel (proc msg) activate)
  (if (memq (process-status proc) '(signal exit))
	  (let ((buffer (process-buffer proc)))
		ad-do-it
		(kill-buffer buffer))
	ad-do-it))

;; we need to scroll
(setq term-buffer-maximum-size 0) 

;;
;; Open new terminal with name
;;
(defun open-terminal (terminal-name)
  "Open terminal with custom buffer name"
  (interactive
   (list
	(read-string "Buffer name: ")))
    (ansi-term (getenv "SHELL") (concat "term: " terminal-name)))
