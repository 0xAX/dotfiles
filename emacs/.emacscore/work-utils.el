(defun migrate-tposs ()
  "migrate-tposs executes migration for the tposs_core application."
  (interactive)
  (let
      ((cwd (file-name-directory buffer-file-name)))
    (cd "/home/alex/work/tposs_core")
    (shell-command-to-string "MIX_ENV=test mix ecto.migrate -r Tposs.Repo")
    (cd cwd))
  (message "Done."))
