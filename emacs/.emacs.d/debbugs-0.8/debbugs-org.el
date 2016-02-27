;;; debbugs-org.el --- Org-mode interface for the GNU bug tracker

;; Copyright (C) 2013-2015 Free Software Foundation, Inc.

;; Author: Michael Albinus <michael.albinus@gmx.org>
;; Keywords: comm, hypermedia, maint, outlines
;; Package: debbugs
;; Version: 0.8

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an interface to bug reports which are located
;; on the GNU bug tracker debbugs.gnu.org.  Its main purpose is to
;; show and manipulate bug reports as org-mode TODO items.

;; If you have `debbugs-org.el' in your load-path, you could enable
;; the bug tracker commands by the following lines in your ~/.emacs
;;
;;   (autoload 'debbugs-org "debbugs-org" "" 'interactive)
;;   (autoload 'debbugs-org-search "debbugs-org" "" 'interactive)
;;   (autoload 'debbugs-org-bugs "debbugs-org" "" 'interactive)

;; The bug tracker is called interactively by
;;
;;   M-x debbugs-org

;; It asks for the severities, for which bugs shall be shown. This can
;; be either just one severity, or a list of severities, separated by
;; comma.  Valid severities are "serious", "important", "normal",
;; "minor" or "wishlist".  Severities "critical" and "grave" are not
;; used, although configured on the GNU bug tracker.  If no severity
;; is given, all bugs are selected.

;; There is also the pseudo severity "tagged".  When it is used, the
;; function will ask for user tags (a comma separated list), and shows
;; just the bugs which are tagged with them.  In general, user tags
;; shall be strings denoting to subprojects of the package, like
;; "cedet" or "tramp" of the package "emacs".  If no user tag is
;; given, locally tagged bugs are shown.

;; If a prefix is given to the command, more search parameters are
;; asked for, like packages (also a comma separated list, "emacs" is
;; the default), whether archived bugs shall be shown, and whether
;; closed bugs shall be shown.

;; Another command is
;;
;;   M-x debbugs-org-search

;; It behaves like `debbugs-org', but asks at the beginning for a
;; search phrase to be used for full text search.  Additionally, it
;; asks for key-value pairs to filter bugs.  Keys are as described in
;; `debbugs-get-status', the corresponding value must be a regular
;; expression to match for.  The other parameters are as described in
;; `debbugs-org'.

;; The bug reports are downloaded from the bug tracker.  In order to
;; not generate too much load of the server, up to 500 bugs will be
;; downloaded at once.  If there are more hits, you will be asked to
;; change this limit, but please don't increase this number too much.

;; These default values could be changed also by customer options
;; `debbugs-gnu-default-severities', `debbugs-gnu-default-packages'
;; and `debbugs-gnu-default-hits-per-page'.

;; The commands create a TODO list.  Besides the usual handling of
;; TODO items, you could apply the following actions by the following
;; keystrokes:

;;   "C-c # C": Send a debbugs control message
;;   "C-c # t": Mark the bug locally as tagged
;;   "C-c # d": Show bug attributes

;; The last entry in a TODO record is the link [[Messages]].  If you
;; follow this link, a Gnus ephemeral group is opened presenting all
;; related messages for this bug.  Here you could also send debbugs
;; control messages by keystroke "C".

;; Finally, if you simply want to list some bugs with known bug
;; numbers, call the command
;;
;;   M-x debbugs-org-bugs

;; The bug numbers to be shown shall be entered as comma separated list.

;;; Code:

(require 'debbugs-gnu)
(require 'org)
(eval-when-compile (require 'cl))

(defconst debbugs-org-severity-priority
  (let ((priority ?A))
    (mapcar
     (lambda (x) (prog1 (cons x (char-to-string priority)) (incf priority)))
     debbugs-gnu-all-severities))
  "Mapping of debbugs severities to TODO priorities.")

(defun debbugs-org-get-severity-priority (state)
  "Returns the TODO priority of STATE."
  (or (cdr (assoc (cdr (assq 'severity state))
		  debbugs-org-severity-priority))
      (cdr (assoc "minor" debbugs-org-severity-priority))))

(defconst debbugs-org-priority-faces
  '(("A" . org-warning)
    ("B" . org-warning))
  "Highlighting of prioritized TODO items.")

;; We do not add the bug numbers list to the elisp:link, because this
;; would be much too long.  Instead, this variable shall keep the bug
;; numbers.
(defvar-local debbugs-org-ids nil
  "The list of bug ids to be shown following the elisp link.")

(defvar debbugs-org-show-buffer-name "*Org Bugs*"
  "The buffer name we present the bug reports.
This could be a temporary buffer, or a buffer linked with a file.")

(defvar debbugs-org-mode) ;; Silence compiler.
(defun debbugs-org-show-buffer-name ()
  "The buffer name we present the bug reports.
This could be a temporary buffer, or a buffer linked with a file."
  (if debbugs-org-mode (buffer-name) debbugs-org-show-buffer-name))

;;;###autoload
(defun debbugs-org-search ()
  "Search for bugs interactively.
Search arguments are requested interactively.  The \"search
phrase\" is used for full text search in the bugs database.
Further key-value pairs are requested until an empty key is
returned."
  (interactive)

  (unwind-protect
      ;; Check for the phrase.
      (let ((phrase (read-string debbugs-gnu-phrase-prompt))
            key val1 severities packages)

	(add-to-list 'debbugs-gnu-current-query (cons 'phrase phrase))

	;; The other queries.
	(catch :finished
	  (while t
	    (setq key (completing-read
		       "Enter attribute: "
		       '("severity" "package" "tags" "submitter" "author"
			 "subject" "status")
		       nil t))
	    (cond
	     ;; Server-side queries.
	     ((equal key "severity")
	      (setq
	       severities
	       (completing-read-multiple
		"Enter severities: " debbugs-gnu-all-severities nil t
		(mapconcat 'identity debbugs-gnu-default-severities ","))))

	     ((equal key "package")
	      (setq
	       packages
	       (completing-read-multiple
		"Enter packages: " debbugs-gnu-all-packages nil t
		(mapconcat 'identity debbugs-gnu-default-packages ","))))

	     ((member key '("tags" "subject"))
	      (setq val1 (read-string (format "Enter %s: " key)))
	      (when (not (zerop (length val1)))
		(add-to-list
		 'debbugs-gnu-current-query (cons (intern key) val1))))

	     ((member key '("submitter" "author"))
	      (when (equal key "author") (setq key "@author"))
	      (setq val1 (read-string "Enter email address: "))
	      (when (not (zerop (length val1)))
		(add-to-list
		 'debbugs-gnu-current-query (cons (intern key) val1))))

	     ((equal key "status")
	      (setq
	       val1
	       (completing-read "Enter status: " '("done" "forwarded" "open")))
	      (when (not (zerop (length val1)))
		(add-to-list
		 'debbugs-gnu-current-query (cons (intern key) val1))))

	     ;; The End.
	     (t (throw :finished nil)))))

	;; Do the search.
	(debbugs-org severities packages))

    ;; Reset query and filter.
    (setq debbugs-gnu-current-query nil)))

;;;###autoload
(defun debbugs-org (severities &optional packages archivedp suppress tags)
  "List all outstanding bugs."
  (interactive
   (let (severities archivedp)
     (list
      (setq severities
	    (completing-read-multiple
	     "Severities: " debbugs-gnu-all-severities nil t
	     (mapconcat 'identity debbugs-gnu-default-severities ",")))
      ;; The next parameters are asked only when there is a prefix.
      (if current-prefix-arg
	  (completing-read-multiple
	   "Packages: " debbugs-gnu-all-packages nil t
	   (mapconcat 'identity debbugs-gnu-default-packages ","))
	debbugs-gnu-default-packages)
      (when current-prefix-arg
	(setq archivedp (y-or-n-p "Show archived bugs?")))
      (when (and current-prefix-arg (not archivedp))
	(y-or-n-p "Suppress unwanted bugs?"))
      ;; This one must be asked for severity "tagged".
      (when (member "tagged" severities)
	(split-string (read-string "User tag(s): ") "," t)))))

  ;; Initialize variables.
  (when (and (file-exists-p debbugs-gnu-persistency-file)
	     (not debbugs-gnu-local-tags))
    (with-temp-buffer
      (insert-file-contents debbugs-gnu-persistency-file)
      (eval (read (current-buffer)))))

  ;; Add queries.
  (dolist (severity (if (consp severities) severities (list severities)))
    (when (not (zerop (length severity)))
      (add-to-list 'debbugs-gnu-current-query (cons 'severity severity))))
  (dolist (package (if (consp packages) packages (list packages)))
    (when (not (zerop (length package)))
      (add-to-list 'debbugs-gnu-current-query (cons 'package package))))
  (when archivedp
    (add-to-list 'debbugs-gnu-current-query '(archive . "1")))
  (when suppress
    (add-to-list 'debbugs-gnu-current-query '(status . "open"))
    (add-to-list 'debbugs-gnu-current-query '(status . "forwarded")))
  (dolist (tag (if (consp tags) tags (list tags)))
    (when (not (zerop (length tag)))
      (add-to-list 'debbugs-gnu-current-query (cons 'tag tag))))

    (unwind-protect
	(with-current-buffer (get-buffer-create (debbugs-org-show-buffer-name))
	  (erase-buffer)

	  (let ((hits debbugs-gnu-default-hits-per-page))
	    (setq debbugs-org-ids
		  (debbugs-gnu-get-bugs debbugs-gnu-current-query))

	    (when (> (length debbugs-org-ids) hits)
	      (let ((cursor-in-echo-area nil))
		(setq hits
		      (string-to-number
		       (read-string
			(format
			 "How many reports (available %d, default %d): "
			 (length debbugs-org-ids) hits)
			nil
			nil
			(number-to-string hits))))))

	    (debbugs-org-show-next-reports hits)))

      ;; Reset query.
      (setq debbugs-gnu-current-query nil)))

(defun debbugs-org-show-reports (bug-numbers)
  "Show bug reports as given in BUG-NUMBERS."
  (pop-to-buffer (get-buffer-create (debbugs-org-show-buffer-name)))
  ;; Local variable `debbugs-org-ids' must survive.
  (let ((doi debbugs-org-ids))
    (org-mode)
    (debbugs-org-mode 1)
    (setq debbugs-org-ids doi))

  (let ((inhibit-read-only t)
	(debbugs-port "gnu.org"))
    (dolist (status
	     (sort
	      (apply 'debbugs-get-status bug-numbers)
	      (lambda (x y) (< (cdr (assq 'id x)) (cdr (assq 'id y))))))
      (let* ((beg (point))
	     (id (cdr (assq 'id status)))
	     (done (string-equal (cdr (assq 'pending status)) "done"))
	     (priority (debbugs-org-get-severity-priority status))
	     (archived (cdr (assq 'archived status)))
	     (tags (append (cdr (assq 'found_versions status))
			   (cdr (assq 'tags status))))
	     (subject (when (cdr (assq 'subject status))
			(decode-coding-string
			 (cdr (assq 'subject status)) 'utf-8)))
	     (date (cdr (assq 'date status)))
	     (last-modified (cdr (assq 'last_modified status)))
	     (originator (when (cdr (assq 'originator status))
			   (decode-coding-string
			    (cdr (assq 'originator status)) 'utf-8)))
	     (owner (when (cdr (assq 'owner status))
		      (decode-coding-string (cdr (assq 'owner status)) 'utf-8)))
	     (closed-by (when (cdr (assq 'done status))
			  (decode-coding-string
			   (cdr (assq 'done status)) 'utf-8)))
	     (merged (cdr (assq 'mergedwith status))))

	;; Handle tags.
	(when (string-match "^\\([0-9.]+\\); \\(.+\\)$" subject)
	  (let ((x (match-string 1 subject))) (pushnew x tags :test #'equal))
	  (setq subject (match-string 2 subject)))
	(when archived
          (pushnew "ARCHIVE" tags :test #'equal))
	(setq tags
	      (mapcar
	       ;; Replace all invalid TAG characters by "_".
	       (lambda (x) (replace-regexp-in-string "[^A-Za-z0-9_@]" "_" x))
	       tags))

	;; Headline.
	(insert
	 (format
	  "* %s [#%s] %s %s\n"
	  (if done "DONE" "TODO")
	  priority subject
	  (if tags (mapconcat 'identity (append '("") tags '("")) ":") "")))

	;; Submitted.
	(when date
	  (insert
	   (format-time-string
	    "  [%Y-%m-%d %a] Submitted\n" (seconds-to-time date))))

	;; Properties.
	(insert "  :PROPERTIES:\n")
	(insert (format "  :DEBBUGS_ID: %s\n" id))
	(when merged
	  (insert
	   (format
	    "  :MERGED_WITH: %s\n"
	    (if (numberp merged)
		merged (mapconcat 'number-to-string merged " ")))))
	(insert (format "  :CREATOR: %s\n" originator))
	(when owner (insert (format "  :OWNER: %s\n" owner)))
	(when closed-by (insert (format "  :CLOSED_BY: %s\n" closed-by)))
	(insert "  :END:\n")

	;; Messages.
	(insert
	 "  [[elisp:(debbugs-gnu-select-report)][Messages]]\n")

	;; Last modified.
	(when last-modified
	  (insert
	   (format-time-string
	    "  [%Y-%m-%d %a] Last modified\n"
	    (seconds-to-time last-modified))))

	;; Add text properties.
	(add-text-properties beg (point) `(tabulated-list-id ,status))))))

(defun debbugs-org-regenerate-status ()
  "Regenerate the `tabulated-list-id' text property.
This property is used when following the [Messages] link, so you
need to regenerate it when opening an .org file after you killed
the corresponding buffer (e.g. by closing Emacs)."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward ":DEBBUGS_ID:[ \t]*\\([0-9]+\\)" nil t)
      (let* ((bugnum (string-to-number (match-string 1)))
	     (mw (org-entry-get (point) "MERGEDWIDTH"))
	     (tli (list (cons 'id bugnum)
			(cons 'bug_num bugnum)
			(cons 'mergedwidth (if mw (string-to-number mw)))))
	    (beg (org-back-to-heading t))
	    (end (org-end-of-subtree t)))
	(add-text-properties beg end `(tabulated-list-id ,tli))))))

(defun debbugs-org-show-next-reports (hits)
  "Show next HITS of bug reports."
  (with-current-buffer (get-buffer-create (debbugs-org-show-buffer-name))
    (save-excursion
      (goto-char (point-max))
      (when (re-search-backward
	     "^* COMMENT \\[\\[elisp:(debbugs-org-show-next-reports" nil t)
	(forward-line -1)
	(delete-region (point) (point-max)))
      (debbugs-org-show-reports
       (butlast debbugs-org-ids (- (length debbugs-org-ids) hits)))
      (setq debbugs-org-ids
	    (last debbugs-org-ids (- (length debbugs-org-ids) hits)))
      (goto-char (point-max))
      (when debbugs-org-ids
	(insert
	 (format
	  "* COMMENT [[elisp:(debbugs-org-show-next-reports %s)][Next bugs]]\n\n"
	  hits)))
      (insert "* COMMENT Local " "Variables\n")
      (when debbugs-org-ids
	(insert "#+NAME: init\n"
		"#+BEGIN_SRC elisp\n"
		(format "(setq debbugs-org-ids '%s)\n" debbugs-org-ids)
		"#+END_SRC\n\n"))
      (insert "# Local " "Variables:\n"
	      "# mode: org\n"
	      "# eval: (debbugs-org-mode 1)\n")
      (when debbugs-org-ids
	(insert (format "# eval: (%s \"init\")\n"
			(if (macrop 'org-sbe) "org-sbe" "sbe"))))
      (insert "# End:\n")
      (goto-char (point-min))
      (org-overview)
      (set-buffer-modified-p nil))))

(defconst debbugs-org-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c # t") 'debbugs-gnu-toggle-tag)
    (define-key map (kbd "C-c # C") 'debbugs-gnu-send-control-message)
    (define-key map (kbd "C-c # d") 'debbugs-gnu-display-status)
    map)
  "Keymap for the `debbugs-org-mode' minor mode.")

;; Make byte-compiler quiet.
(defvar gnus-posting-styles)

;;;###autoload
(define-minor-mode debbugs-org-mode
  "Minor mode for providing a debbugs interface in org-mode buffers.

\\{debbugs-org-mode-map}"
  :lighter " Debbugs" :keymap debbugs-org-mode-map
  ;; FIXME: Does not show any effect.
  (set (make-local-variable 'org-priority-faces) debbugs-org-priority-faces)
  (set (make-local-variable 'gnus-posting-styles)
       `((".*"
	  (eval
	   (when (buffer-live-p gnus-article-copy)
	     (with-current-buffer gnus-article-copy
	       (set (make-local-variable 'message-prune-recipient-rules)
		    '((".*@debbugs.*" "emacs-pretest-bug")
		      (".*@debbugs.*" "bug-gnu-emacs")
		      ("[0-9]+@debbugs.*" "submit@debbugs.gnu.org")
		      ("[0-9]+@debbugs.*" "quiet@debbugs.gnu.org")))
	       ;; `gnus-posting-styles' is eval'ed after
	       ;; `message-simplify-subject'.  So we cannot use m-s-s.
	       (setq subject ,debbugs-gnu-subject)))))))
  (debbugs-org-regenerate-status))

;;;###autoload
(defun debbugs-org-bugs (&rest bugs)
  "List all BUGS, a list of bug numbers."
  (interactive
   (mapcar 'string-to-number
	   (completing-read-multiple "Bug numbers: " nil 'natnump)))
  (dolist (elt bugs)
    (unless (natnump elt) (signal 'wrong-type-argument (list 'natnump elt))))
  (add-to-list 'debbugs-gnu-current-query (cons 'bugs bugs))
  (debbugs-org nil))

;; TODO

;; - Refactor it in order to avoid code duplication with debbugs-gnu.el.
;; - Make headline customizable.
;; - Sort according to different TODO properties.

(provide 'debbugs-org)
