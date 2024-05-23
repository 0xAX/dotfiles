(defun print-list-of-strings (strings)
  "Print each string in STRINGS to the *Messages* buffer."
  (dolist (string strings)
    (message "%s" string)))
