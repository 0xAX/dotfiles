(require 'mu4e)
(require 'smtpmail)

;; mail user agent will be used to send email(s)
(setq mail-user-agent 'mu4e-user-agent)

;; format for citations
(setq message-citation-line-function 'message-insert-formatted-citation-line) 
(setq message-citation-line-format "On %a, %b %d %Y, %f wrote:\n")

;; Main maildir
(setq mu4e-maildir "~/disk/Maildir/kuleshovmail")

;; some folders
;;(setq mu4e-sent-folder "/home/alex/disk/Maildir/kuleshovmail/sent"
;;      mu4e-drafts-folder "/home/alex/disk/Maildir/kuleshovmail/drafts")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; update interval for fetching email (seconds)
(setq mu4e-update-interval 120)

;; shortcuts for mail dirs
(setq mu4e-maildir-shortcuts
      '( ("/INBOX"               . ?i)
         ("/moby"                 . ?m)
         ("/k8s"                  . ?8)
         ("/AWS"                  . ?a)
         ("/Elixir"               . ?e)
         ("/Erlang"               . ?E)
         ("/Kernel"               . ?k)
         ("/mysql"                . ?M)
         ("/OpenBSD"              . ?O)
         ("/Travelping"           . ?T)
         ("/xerions"              . ?x)))

;; allow for updating mail using 'U' in the main view
(setq mu4e-get-mail-command "offlineimap")

;; some info about me
(setq
   user-mail-address "kuleshovmail@gmail.com"
   user-full-name  "Alexander Kuleshov"
   mu4e-compose-signature
    (concat
      "Best regards\n"
      "Alex Kuleshov\n"))

;; smtp configuration
(setq message-send-mail-function 'smtpmail-send-it
   starttls-use-gnutls t
   smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
   smtpmail-auth-credentials
     '(("smtp.gmail.com" 587 "kuleshovmail@gmail.com" nil))
   smtpmail-default-smtp-server "smtp.gmail.com"
   smtpmail-smtp-server "smtp.gmail.com"
   smtpmail-smtp-service 587)

