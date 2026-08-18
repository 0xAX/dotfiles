;;; .emacs --- My org-mode configuration  -*- lexical-binding: t -*-

(require 'org)
;; show a consistency graph in the agenda for repeating tasks
;; that have the :STYLE: habit property
(require 'org-habit)

;; set path to org directory
(when (file-directory-p "~/dev/todo")
  (setq org-directory "~/dev/todo")
  (setq org-agenda-files '("~/dev/todo/agenda.org.gpg")))
(when (file-directory-p "~/disk/dev/todo")
  (setq org-directory "~/disk/dev/todo")
  (setq org-agenda-files '("~/disk/dev/todo/agenda.org.gpg")))

;; set path to reading dir
(defvar org-reading-directory
  (expand-file-name "reading/" org-directory))

(defun org-reading-files ()
  "Return every Org file under `org-reading-directory', recursively.
Putting the directory itself into `org-agenda-files' is not enough: org
expands a directory with `directory-files', which is not recursive and
which uses `org-agenda-file-regexp', so it would miss both the notes in
subdirectories (essays/, notes/, posts/, tech/) and the encrypted
`.org.gpg' ones."
  (directory-files-recursively org-reading-directory
                               "\\.org\\(\\.gpg\\)?\\'"))

;; Needed for `epa-file-passphrase-alist' and
;; `epa-file-cache-passphrase-for-symmetric-encryption' below: without epa-file
;; loaded neither symbol is special, so under `lexical-binding' the `let' in
;; `org-reading-call' would bind them lexically and epa would never see them.
(require 'epa-file)

(defvar org-reading-passphrase nil
  "Passphrase for the symmetrically encrypted reading notes.
Kept in memory for this Emacs session only; drop it with
`org-reading-forget-passphrase'.")

(defun org-reading-forget-passphrase ()
  "Drop the cached passphrase for the reading notes."
  (interactive)
  (setq org-reading-passphrase nil)
  (message "Reading-notes passphrase forgotten"))

(defun org-reading-passphrase-alist ()
  "Return an `epa-file-passphrase-alist' covering every encrypted reading note.
The notes are encrypted symmetrically, so each file carries its own
random S2K salt.  That defeats both caches: gpg-agent keys its symkey
cache by that salt, and `epa-file-passphrase-callback-function' keys
`epa-file-passphrase-alist' by `file-truename'.  Neither key can be
shared between files, so a tags search over the reading tree asks for a
passphrase once per file.  The files do share a single passphrase, so
asking for it one time and pre-filling an entry for every file collapses
those prompts into one."
  (unless org-reading-passphrase
    (setq org-reading-passphrase
          (read-passwd "Passphrase for reading notes: ")))
  (mapcar (lambda (file)
            (cons (file-truename file)
                  (copy-sequence org-reading-passphrase)))
          (seq-filter (lambda (file) (string-suffix-p ".gpg" file))
                      (org-reading-files))))

(defun org-reading-call (fn)
  "Call FN with the reading notes bound to `org-agenda-files'.
Asks for their passphrase once instead of once per encrypted file."
  (let ((epa-file-cache-passphrase-for-symmetric-encryption t)
        (epa-file-passphrase-alist (org-reading-passphrase-alist))
        (org-agenda-files (org-reading-files)))
    (condition-case err
        (funcall fn)
      ;; A rejected passphrase must not stay cached for the rest of the session.
      (error
       (setq org-reading-passphrase nil)
       (signal (car err) (cdr err))))))

;; Org-agenda settings
(setq
 ;; the default agenda starts tomorrow
 org-agenda-start-day "+1d"
 ;; and spans 7 days
 org-agenda-span 7
 ;; starting from the current day instead of the beginning of the week
 org-agenda-start-on-weekday nil
 ;; add a timestamp when a task is marked as DONE
 org-log-done 'time
 ;; archive entries (C-c C-x C-a) into a datetree in archive.org.gpg,
 ;; grouped by the date they were archived
 org-archive-location (concat org-directory "/archive.org.gpg::datetree/")
 ;; make agenda text search (C-c o a s) also look through archived entries
 org-agenda-text-search-extra-files '(agenda-archives)
 ;; show habits on every day of multi-day agenda views, not only on today
 org-habit-show-habits-only-for-today nil
 ;; Custom agenda views, available via C-c o a followed by d/3/w/m/q
 ;; (look forward) or -1/-3 (look back one/three months)
 org-agenda-custom-commands
 `(("r" "Reading notes by tag"
   tags
   ""
   ((org-agenda-files (org-reading-files))
    (epa-file-cache-passphrase-for-symmetric-encryption t)
    (epa-file-passphrase-alist (org-reading-passphrase-alist))))
   ;; today only
   ("d" "Today" agenda ""
    ((org-agenda-span 'day)
     (org-agenda-start-day nil)))
   ;; 3 days centered on today
   ("3" "Yesterday, today, tomorrow" agenda ""
    ((org-agenda-span 3)
     (org-agenda-start-day "-1d")))
   ;; 7 days starting today
   ("w" "Next 7 days" agenda ""
    ((org-agenda-span 7)
     (org-agenda-start-day nil)))
   ;; 30 days starting today
   ("m" "Next 30 days" agenda ""
    ((org-agenda-span 30)
     (org-agenda-start-day nil)))
   ;; 90 days starting today
   ("q" "Next 3 months" agenda ""
    ((org-agenda-span 90)
     (org-agenda-start-day nil)))
   ;; past 30 days ending today, with log mode to show closed tasks
   ("-1" "Past 30 days" agenda ""
    ((org-agenda-span 30)
     (org-agenda-start-day "-29d")
     (org-agenda-start-with-log-mode t)))
   ;; past 90 days ending today, with log mode to show closed tasks
   ("-3" "Past 3 months" agenda ""
    ((org-agenda-span 90)
     (org-agenda-start-day "-89d")
     (org-agenda-start-with-log-mode t)))))

;; Capture new tasks with C-c c into the agenda file, under Personal:
;;   t - task scheduled for today
;;   T - task scheduled for a date picked in the calendar
(setq org-default-notes-file (car org-agenda-files))
(setq org-capture-templates
      '(("t" "Task for today" entry
         (file+headline org-default-notes-file "Personal")
         "* TODO %?\nSCHEDULED: %t")
        ("T" "Task for a date" entry
         (file+headline org-default-notes-file "Personal")
         "* TODO %?\nSCHEDULED: %^t")))

;; Enable org-mode for *.org files
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; Add ability to follow links by pressing RET
(setq org-return-follows-link  t)

;; Some good defaults
(setq-default
 org-startup-indented t
 line-spacing 1
 org-src-fontify-natively t
 org-fontify-quote-and-verse-block t
 org-pretty-entities t
 org-startup-with-inline-images t
 org-hide-emphasis-markers t
 org-src-tab-acts-natively t
 org-src-preserve-indentation 2
 org-edit-src-content-indentation 2
 org-edit-src-content-indentation 0
 org-src-fontify-natively t
 org-src-window-setup 'current-window
 org-src-strip-leading-and-trailing-blank-lines t
 org-src-preserve-indentation t
 org-src-tab-acts-natively t)

;; Enable auto-search in org-mode so any non-standard keypress in C-x C-j
;; mode will lead to search according to pressed keys
(setq org-goto-auto-isearch t)

;; Enable pretty printing of special symbols
(org-toggle-pretty-entities)

;; Enable support for org-tables everywhere
(add-hook 'message-mode-hook 'turn-on-orgtbl)

(defun my-reading-tags ()
  "Search reading notes by Org tags."
  (interactive)
  (org-reading-call
   (lambda () (call-interactively #'org-tags-view))))

;; load additional org-mode helpers
(load "~/.emacscore/org/org-api.el")
(load "~/.emacscore/org/org-babel.el")
(load "~/.emacscore/org/org-ui.el")
(load "~/.emacscore/org/org-keybindings.el")
(load "~/.emacscore/org/org-markdown.el")
(load "~/.emacscore/org/org-latex.el")

(add-hook 'org-mode-hook 'org-fragtog-mode)
