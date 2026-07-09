;;; dired.el --- My configuration for email  -*- lexical-binding: t -*-

(when (file-directory-p "/usr/local/share/emacs/site-lisp/mu4e")
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")

  ;; Load mu4e
  (require 'mu4e)

  ;; use mu4e for e-mail in emacs
  (setq mail-user-agent 'mu4e-user-agent)

  ;; Set the common folders
  (setq mu4e-drafts-folder "/[Gmail].Drafts")
  (setq mu4e-sent-folder   "/[Gmail].Sent Mail")
  (setq mu4e-trash-folder  "/[Gmail].Trash")

  ;; don't save message to Sent Messages, Gmail/IMAP takes care of this
  (setq mu4e-sent-messages-behavior 'delete)

  ;; Make 'd' (trash) behave like delete in the Gmail web UI: just move the
  ;; message to the Trash folder. Without this, mu4e also adds the maildir
  ;; +T (trashed) flag, which offlineimap syncs to Gmail as \Deleted -- and
  ;; Gmail then expunges the message entirely instead of keeping it in Trash
  ;; for 30 days.
  (setq mu4e-trash-without-flag t)

  (setq mu4e-maildir-shortcuts
        '( (:maildir "/INBOX"               :key ?i)
           (:maildir "/Elixir"              :key ?e)
           (:maildir "/Emacs"               :key ?E)
           (:maildir "/Erlang"              :key ?o)
           (:maildir "/Github-Repos"        :key ?g)
           (:maildir "/NetBSD"              :key ?N)
           (:maildir "/OpenBSD"             :key ?O)           
           (:maildir "/Kernel"              :key ?k)
           (:maildir "/Postgresql"          :key ?p)
           (:maildir "/Travelping"          :key ?w)           
           (:maildir "/[Gmail].Spam"        :key ?S)           
           (:maildir "/[Gmail].Sent Mail"   :key ?s)
           (:maildir "/[Gmail].Trash"       :key ?t)
           (:maildir "/[Gmail].All Mail"    :key ?a)))
  (add-to-list 'mu4e-bookmarks
               '(:query "maildir:/inbox" :name "Inbox" :key ?i :favorite t))

  ;; Mark all messages in the headers view whose subject matches a regexp
  ;; for trash, in one go. Bound to 'D' below; execute with 'x' as usual.
  ;; (This shadows the default 'D' = permanent delete mark, which is still
  ;; reachable via '%' or '&'.)
  (defun trash-all-by-subject (pattern)
    "Mark all visible messages whose subject matches PATTERN for trash.
Messages that are already in the trash folder are marked for
permanent deletion instead, mirroring how deleting from Trash
works in the Gmail web UI."
    (interactive
     (list (read-string (mu4e-format "Trash all with subject matching: ")
                        nil 'mu4e~headers-regexp-hist)))
    (let ((count 0)
          (case-fold-search t))
      (mu4e-headers-for-each
       (lambda (msg)
         (let ((subject (mu4e-message-field msg :subject)))
           (when (and subject (string-match-p pattern subject))
             (mu4e-mark-at-point
              (if (string= (mu4e-message-field msg :maildir) mu4e-trash-folder)
                  'delete
                'trash)
              nil)
             (cl-incf count)))))
      (message "%d message(s) marked" count)))

  (define-key mu4e-headers-mode-map (kbd "D") #'trash-all-by-subject)

  ;; allow for updating mail using 'U' in the main view:
  (setq mu4e-get-mail-command "sync-email")

  ;; something about ourselves
  (setq
   user-mail-address "kuleshovmail@gmail.com"
   user-full-name  "Alex Kuleshov")

  ;; SMTP configuration
  (require 'smtpmail)
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials
        '(("smtp.gmail.com" 587 "kuleshovmail@gmail.com" nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587)

  (setq smtpmail-queue-mail nil 
        smtpmail-queue-dir   "~/Mail/queue/cur")

  ;; don't keep message buffers around
  (setq message-kill-buffer-on-exit t)

  ;; Enable interactive password for the 'U'
  (setq mu4e--get-mail-password-regexp "^Enter the password for kuleshovmail@gmail.com.*$"))

