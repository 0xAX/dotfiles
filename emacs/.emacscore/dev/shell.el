(defun shell-indentation ()
  (interactive)
  (setq sh-basic-offset 4
        sh-indentation  4
        sh-indent-for-case-label 0
        sh-indent-for-case-alt '+))

(add-hook 'sh-mode-hook 'shell-indentation)
