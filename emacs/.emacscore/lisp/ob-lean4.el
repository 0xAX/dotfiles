;;; ob-lean4.el --- Org-babel support for Lean 4  -*- lexical-binding: t -*-

;; Copyright (C) 2026 Alex Kuleshov

;; Author: Alex Kuleshov <kuleshovmail@gmail.com>
;; URL: https://github.com/0xAX/ob-lean
;; Keywords: lisp
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Org does not ship an `ob-' backend for Lean, so this file provides one
;; for Lean 4. A block is compiled by writing it to a temporary file and
;; running `lean --json' over it.  The JSON messages Lean emits (the results
;; of `#eval' and `#check', plus warnings and errors) become the result of
;; the block.
;;
;; Lean 4 has no REPL, so a session is not a live process.  `:session' is
;; supported anyway, in the only way it can be: the blocks already run in
;; a named session are compiled again in front of the block being run, so
;; that what they defined is in scope.  Only the messages the current
;; block produced become its result.  The `#eval' output of the blocks
;; replayed in front of it is dropped, since it was already reported when
;; they were run.  `org-babel-lean4-clear-session' forgets a session.
;;
;; Imports can simply be written at the top of the block, the way they are
;; in a Lean file.  Lean insists on seeing them before anything else, so
;; whatever `:imports', `:var' and `:prologue' generate is slotted in
;; underneath them rather than above.
;;
;; Header arguments, in addition to the standard ones:
;;
;;   :imports  - space or comma separated module names turned into `import'
;;               lines.  Convenient when the same modules are wanted by
;;               many blocks, or from a `#+PROPERTY:' line.  Writing the
;;               `import' in the block does the same thing.
;;   :lake     - "yes" runs the block as `lake env lean' instead of `lean',
;;               which makes the dependencies of a Lake project (Mathlib,
;;               say) importable.  Combine it with the standard `:dir' to
;;               point at the project root.
;;   :flags    - extra command line flags passed to `lean'.
;;   :messages - which of Lean's messages end up in the result:
;;               "all" (default), "info" (only `#eval'/`#check' output),
;;               "diag" (only warnings and errors) or "none".
;;   :positions - whether a message is prefixed with the line and column it
;;               came from: "diag" (default, only warnings and errors),
;;               "yes" or "no".  Line numbers are relative to the block
;;               body, so whatever `:imports' and `:var' added is not
;;               counted.
;;   :session  - name of a session whose blocks are compiled in front of
;;               this one.  A block only joins its session once it compiles
;;               without errors, and running it again replaces what it
;;               contributed rather than adding a second copy.
;;
;; A block that compiles cleanly and says nothing produces no result at
;; all, so definition-only blocks do not litter the document with empty
;; `#+RESULTS:' drawers.

;;; Code:

(require 'ob)
(require 'seq)
(require 'subr-x)

(defcustom org-babel-lean4-command-name "lean"
  "Name of the Lean executable used to run src blocks."
  :group 'org-babel
  :type 'string)

(defcustom org-babel-lean4-lake-name "lake"
  "Name of the Lake executable used for `:lake yes' src blocks."
  :group 'org-babel
  :type 'string)

(defvar org-babel-default-header-args:lean4
  '((:results . "output") (:exports . "both"))
  "Default header arguments for Lean 4 src blocks.
Everything Lean has to say about a block arrives on its standard
output, so `output' is the only meaningful `:results' type.")

(defconst org-babel-header-args:lean4
  '((imports   . :any)
    (lake      . ((yes no)))
    (flags     . :any)
    (messages  . ((all info diag none)))
    (positions . ((yes diag no))))
  "Lean 4 specific header arguments.")

(defconst org-babel-lean4--severity-labels
  '((error       . "error")
    (warning     . "warning")
    (information . "info"))
  "How Lean's message severities are spelled in the result.")

;;; Header arguments

(defun org-babel-lean4--header (params key default)
  "Return the value of the KEY header argument in PARAMS, or DEFAULT.

Org hands a header argument over as whatever was written after it, so the
value can arrive as a string or, from a `:var' or a default set in Lisp,
as a symbol.  It is normalised to a downcased string here, so that the
rest of the file can compare it with `equal' and `:messages Diag' means
what it says."
  (let ((value (cdr (assq key params))))
    (if (or (null value) (equal value ""))
        default
      (downcase (format "%s" value)))))

;;; Locating the toolchain

(defun org-babel-lean4--executable (name)
  "Return the full path of the Lean toolchain executable NAME.
Prefers whatever `lean4-mode' is configured to use, so that a block and
the language server always run the same toolchain."
  (cond
   ;; `lean4-get-executable' honours `lean4-rootdir', which is usually
   ;; pointed at the elan root so that the toolchain a project pins is the
   ;; one that gets run.
   ((fboundp 'lean4-get-executable) (lean4-get-executable name))
   ((and (boundp 'lean4-rootdir) (stringp lean4-rootdir))
    (expand-file-name name (expand-file-name "bin" lean4-rootdir)))
   ((executable-find name))
   (t (error "Cannot find the Lean toolchain executable `%s'; set `lean4-rootdir'"
             name))))

;;; Expanding the block

(defun org-babel-lean4-var-to-lean4 (value)
  "Render the Emacs Lisp VALUE as a Lean 4 literal."
  (cond
   ((eq value t) "true")
   ((null value) "[]")
   ((numberp value) (number-to-string value))
   ((stringp value) (format "%S" value))
   ((symbolp value) (symbol-name value))
   ((or (listp value) (vectorp value))
    (concat "["
            (mapconcat #'org-babel-lean4-var-to-lean4
                       (append value nil) ", ")
            "]"))
   (t (format "%S" (format "%s" value)))))

(defun org-babel-lean4--import-lines (params)
  "Return the `import' lines requested by the `:imports' header in PARAMS."
  (let ((imports (cdr (assq :imports params))))
    (when (org-string-nw-p imports)
      (mapcar (lambda (module) (concat "import " module))
              (split-string imports "[ \t,]+" t)))))

(defun org-babel-lean4--var-lines (params)
  "Return a `def' line for every `:var' in PARAMS."
  (mapcar (lambda (pair)
            (format "def %s := %s"
                    (car pair)
                    (org-babel-lean4-var-to-lean4 (cdr pair))))
          (org-babel--get-vars params)))

(defun org-babel-lean4--split-imports (body)
  "Split BODY into a cons (HEAD . REST) around its own `import' lines.

HEAD is the run of `import' lines BODY opens with, together with any
blank lines and comments among them.  REST is everything below.  Lean
only accepts `import' at the very top of a file, so anything this file
generates has to go after HEAD rather than in front of it."
  (let ((lines (split-string body "\n"))
        (index 0)
        (last-import -1)
        (scanning t))
    (while (and scanning (< index (length lines)))
      (let ((line (string-trim (nth index lines))))
        (cond
         ((string-match-p "\\`import\\(?:[[:space:]]\\|\\'\\)" line)
          (setq last-import index))
         ((or (string-empty-p line) (string-prefix-p "--" line)))
         (t (setq scanning nil))))
      (setq index (1+ index)))
    (if (< last-import 0)
        (cons "" body)
      (cons (mapconcat #'identity (seq-take lines (1+ last-import)) "\n")
            (mapconcat #'identity (seq-drop lines (1+ last-import)) "\n")))))

(defun org-babel-lean4--pieces (body params)
  "Take BODY apart into the pieces the compiled file is built from, per PARAMS.

Return a list (IMPORTS HEAD-LINES GENERATED CHUNK).  IMPORTS are the
lines that have to sit at the very top of the file, because Lean accepts
`import' nowhere else: the ones BODY opens with, followed by the ones
`:imports' asks for.  HEAD-LINES is how many of them came from BODY
itself.  CHUNK is everything else - what `:prologue' and `:var' generate,
then the rest of BODY, then `:epilogue' - and GENERATED is how many lines
of it were generated rather than written in the block."
  (pcase-let* ((`(,head . ,rest) (org-babel-lean4--split-imports body))
               (head-lines (when (org-string-nw-p head)
                             (split-string head "\n")))
               (generated (append
                           (let ((prologue (cdr (assq :prologue params))))
                             (when (org-string-nw-p prologue)
                               (split-string prologue "\n")))
                           (org-babel-lean4--var-lines params)))
               (epilogue (cdr (assq :epilogue params))))
    (list (append head-lines (org-babel-lean4--import-lines params))
          (length head-lines)
          (length generated)
          (mapconcat #'identity
                     (append generated
                             (list rest)
                             (when (org-string-nw-p epilogue) (list epilogue)))
                     "\n"))))

(defun org-babel-expand-body:lean4 (body params)
  "Expand BODY into the Lean 4 file that will be compiled, per PARAMS.
The other blocks of a `:session' are not part of the expansion.  They are
put in front of the block when it is run, and repeating them here would
copy them into everything the block is tangled into."
  (pcase-let ((`(,imports ,_head-lines ,_generated ,chunk)
               (org-babel-lean4--pieces body params)))
    (mapconcat #'identity
               (append imports (list chunk))
               "\n")))

(defun org-babel-lean4--body-line (line layout)
  "Map LINE of the compiled file back onto the block body.

LAYOUT says where in the file the block ended up.  It is the plist
`org-babel-execute:lean4' builds while assembling the file.  The answer
is the line within the block body, 0 for a line generated from
`:imports', `:prologue' or `:var', and nil for a line that belongs to
another block of the session rather than to this one."
  (let ((head-start (plist-get layout :head-start))
        (head-lines (plist-get layout :head-lines))
        (imports-end (plist-get layout :imports-end))
        (chunk-start (plist-get layout :chunk-start))
        (generated (plist-get layout :generated)))
    (cond
     ;; The block's own `import' lines, hoisted into the import section but
     ;; kept together, so they can still be pointed at individually.
     ((and (> head-lines 0)
           (>= line head-start)
           (< line (+ head-start head-lines)))
      (1+ (- line head-start)))
     ;; What `:imports' added, which sits right behind them.
     ((and (>= line (+ head-start head-lines)) (<= line imports-end)) 0)
     ;; An import of some other block, or one of the blocks themselves.
     ((< line chunk-start) nil)
     (t (let ((chunk-line (1+ (- line chunk-start))))
          (if (<= chunk-line generated)
              0
            (+ (- chunk-line generated) head-lines)))))))

;;; Sessions

(defvar org-babel-lean4--sessions (make-hash-table :test #'equal)
  "The blocks that have been run in each named session.

Maps a session name to a cons (IMPORTS . CHUNKS): the import section the
session has accumulated, and an alist of (KEY . TEXT) holding the body of
every block that has joined it, in the order they were run.  Lean 4 has
no REPL, so this is all a session can be - the source of what came
before, compiled again in front of the block being run.")

(defun org-babel-lean4--session-name (params)
  "Return the session PARAMS asks for, or nil if the block has none."
  (let ((session (cdr (assq :session params))))
    (and (org-string-nw-p session)
         (not (equal session "none"))
         session)))

(defun org-babel-lean4--session-state (session)
  "Return the (IMPORTS . CHUNKS) cons SESSION has accumulated so far."
  (or (gethash session org-babel-lean4--sessions) (cons nil nil)))

(defun org-babel-lean4--block-index (position)
  "Return the number of src blocks that begin before POSITION."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (let ((case-fold-search t)
            (count 0))
        (while (re-search-forward "^[ \t]*#\\+begin_src\\b" position t)
          (setq count (1+ count)))
        count))))

(defun org-babel-lean4--block-key ()
  "Return a key identifying the src block being executed.

Running a block a second time has to replace what it gave its session
rather than compile another copy of its definitions, which Lean would
reject.  Blocks are told apart by their `#+name:' when they have one, and
otherwise by their position among the src blocks of the buffer - a count
that, unlike a buffer position, survives results being rewritten above
them."
  (let* ((info (ignore-errors (org-babel-get-src-block-info 'light)))
         (name (org-string-nw-p (nth 4 info))))
    (or name
        (format "%s#%d"
                (or (buffer-file-name) (buffer-name))
                (org-babel-lean4--block-index (or (nth 5 info) (point)))))))

(defun org-babel-lean4--session-record (session key imports chunk)
  "Remember that the block KEY contributed IMPORTS and CHUNK to SESSION."
  (let* ((state (org-babel-lean4--session-state session))
         (chunks (cdr state))
         (cell (assoc key chunks)))
    (if cell
        (setcdr cell chunk)
      (setq chunks (append chunks (list (cons key chunk)))))
    (puthash session (cons imports chunks) org-babel-lean4--sessions)))

(defun org-babel-lean4--import-section (prior own)
  "Merge the OWN import lines of a block with the PRIOR ones of its session.

Return a cons (LINES . OWN-START), where OWN-START is the line LINES
reaches OWN at: the block's own imports are kept together, and at the
end, so that Lean's complaints about them can still be pointed back at
the lines they were written on.  An import a previous block already asked
for is therefore dropped from PRIOR rather than from OWN."
  (let ((keep (seq-remove (lambda (line) (member line own)) prior)))
    (cons (append keep own) (1+ (length keep)))))

(defun org-babel-lean4-clear-session (session)
  "Forget the blocks accumulated in SESSION.
With an empty SESSION, forget every session.  Useful once a block has
been edited or deleted: the session still holds the version of it that
was run, and Lean will refuse a definition that arrives twice."
  (interactive
   (list (completing-read "Clear Lean session (empty for all): "
                          (hash-table-keys org-babel-lean4--sessions))))
  (if (org-string-nw-p session)
      (remhash session org-babel-lean4--sessions)
    (clrhash org-babel-lean4--sessions))
  (when (called-interactively-p 'interactive)
    (message "ob-lean4: cleared %s"
             (if (org-string-nw-p session)
                 (format "session `%s'" session)
               "all sessions"))))

;;; Running Lean

(defun org-babel-lean4--run (file params)
  "Run Lean over FILE according to PARAMS.
Return a cons cell (EXIT-CODE . OUTPUT).  Lean writes its diagnostics to
standard output, and standard error is merged into it so that a crash or
a Lake failure is not silently dropped."
  (let* ((lake (member (org-babel-lean4--header params :lake "no")
                       '("yes" "t" "true")))
         (program (org-babel-lean4--executable
                   (if lake org-babel-lean4-lake-name
                     org-babel-lean4-command-name)))
         (flags (cdr (assq :flags params)))
         (args (append (when lake (list "env" org-babel-lean4-command-name))
                       (list "--json")
                       (when (org-string-nw-p flags)
                         (split-string flags))
                       (list (file-local-name file)))))
    (with-temp-buffer
      (let ((exit (apply #'process-file program nil '(t t) nil args)))
        (cons exit (buffer-string))))))

(defun org-babel-lean4--parse (output layout)
  "Parse the `lean --json' OUTPUT into a list of messages.
Each message is a list (SEVERITY LINE COLUMN TEXT), with LINE mapped back
onto the block body by `org-babel-lean4--body-line' given LAYOUT.  A line
that is not valid JSON - Lake noise, a panic - is kept verbatim with
severity `raw', so nothing Lean printed can go missing."
  (let (messages)
    (dolist (line (split-string output "\n") (nreverse messages))
      (unless (string-empty-p (string-trim line))
        (push
         (condition-case nil
             (let* ((msg (json-parse-string line
                                            :object-type 'alist
                                            :null-object nil
                                            :false-object nil))
                    (pos (alist-get 'pos msg))
                    (severity (alist-get 'severity msg)))
               (list (intern (or severity "information"))
                     ;; A message Lean did not place - a failed import, say
                     ;; - belongs to the block as much as to anything else.
                     (if pos
                         (org-babel-lean4--body-line
                          (or (alist-get 'line pos) 0) layout)
                       0)
                     (or (alist-get 'column pos) 0)
                     (string-trim-right (or (alist-get 'data msg) ""))))
           (error (list 'raw nil nil line)))
         messages)))))

(defun org-babel-lean4--keep-p (severity filter)
  "Return non-nil if a message of SEVERITY passes FILTER.
Messages Lean did not tag - severity `raw' - always pass."
  (cond
   ((eq severity 'raw) t)
   ((equal filter "none") nil)
   ((equal filter "info") (eq severity 'information))
   ((equal filter "diag") (memq severity '(error warning)))
   (t t)))

(defun org-babel-lean4--prefix-p (severity mode)
  "Return non-nil if a message of SEVERITY should carry its position.
MODE is the value of the `:positions' header argument."
  (cond
   ((eq severity 'raw) nil)
   ((equal mode "no") nil)
   ((equal mode "yes") t)
   (t (memq severity '(error warning)))))

(defun org-babel-lean4--render (messages params)
  "Render MESSAGES as the result text of a src block, honouring PARAMS."
  (let ((filter (org-babel-lean4--header params :messages "all"))
        (positions (org-babel-lean4--header params :positions "diag"))
        lines)
    (pcase-dolist (`(,severity ,line ,column ,text) messages)
      (when (and (org-babel-lean4--keep-p severity filter)
                 ;; The blocks replayed in front of this one print their
                 ;; `#eval' output again every time the session is
                 ;; compiled. It was reported when they were run, so only
                 ;; what went wrong in them is worth repeating here.
                 (not (and (null line) (eq severity 'information))))
        (push (if (org-babel-lean4--prefix-p severity positions)
                  (format "%s %s: %s"
                          ;; A line of 0 means the message came from what
                          ;; `:imports', `:var' or `:prologue' put in front
                          ;; of the block. nil, that it came from one of the
                          ;; earlier blocks of the session.
                          (cond
                           ((null line) "session")
                           ((> line 0) (format "L%d:%d" line column))
                           (t "preamble"))
                          (alist-get severity org-babel-lean4--severity-labels
                                     (symbol-name severity))
                          text)
                text)
              lines)))
    (mapconcat #'identity (nreverse lines) "\n")))

(defvar org-babel-lean4--silence-result nil
  "Non-nil when the Lean block just executed had nothing to report.")

(defun org-babel-lean4--remove-empty-result ()
  "Remove the empty result drawer left behind by a silent Lean block.
Returning nil from `org-babel-execute:lean4' is not enough: Org inserts a
`#+RESULTS:' line anyway, which would sit under every block that only
defines things - most of them, in a Lean document."
  (when org-babel-lean4--silence-result
    (setq org-babel-lean4--silence-result nil)
    (ignore-errors (org-babel-remove-result))))

;; Depth -100 puts this in front of anything else on the hook, so that a
;; hook decorating `#+RESULTS:' does not annotate a drawer we are about to
;; delete and leave its decoration behind.
(add-hook 'org-babel-after-execute-hook
          #'org-babel-lean4--remove-empty-result -100)

(defun org-babel-execute:lean4 (body params)
  "Execute the Lean 4 src block BODY with header arguments PARAMS.
Called by `org-babel-execute-src-block'."
  (setq org-babel-lean4--silence-result nil)
  (pcase-let* ((session (org-babel-lean4--session-name params))
               (state (if session
                          (org-babel-lean4--session-state session)
                        (cons nil nil)))
               (`(,own-imports ,head-lines ,generated ,chunk)
                (org-babel-lean4--pieces body params))
               (`(,imports . ,head-start)
                (org-babel-lean4--import-section (car state) own-imports))
               ;; The blocks of the session, compiled again so that what
               ;; they defined is in scope for this one.
               (prelude (mapconcat #'cdr (cdr state) "\n"))
               (prelude-lines (if (string-empty-p prelude)
                                  0
                                (length (split-string prelude "\n"))))
               (layout (list :head-start head-start
                             :head-lines head-lines
                             :imports-end (length imports)
                             :chunk-start (+ (length imports) prelude-lines 1)
                             :generated generated))
               (file (org-babel-temp-file "lean4-" ".lean")))
    (with-temp-file file
      (insert (mapconcat #'identity
                         (append imports
                                 (unless (string-empty-p prelude)
                                   (list prelude))
                                 (list chunk))
                         "\n")))
    (pcase-let* ((`(,exit . ,output) (org-babel-lean4--run file params))
                 (messages (org-babel-lean4--parse output layout))
                 (rendered (org-babel-lean4--render messages params)))
      ;; A block joins its session only once it compiles, so that a typo
      ;; does not have to be undone by hand before the next block can run.
      (when (and session
                 (zerop exit)
                 (not (seq-some (lambda (message) (eq (car message) 'error))
                                messages)))
        (org-babel-lean4--session-record
         session (org-babel-lean4--block-key) imports chunk))
      (cond
       ;; Something went wrong and Lean said nothing we could parse: report
       ;; the status rather than pretending the block succeeded.
       ((and (not (zerop exit)) (string-empty-p rendered))
        (format "Lean exited with status %d" exit))
       ;; Nothing to show: leave the document alone instead of inserting an
       ;; empty results drawer.
       ((string-empty-p rendered)
        (setq org-babel-lean4--silence-result t)
        nil)
       (t rendered)))))

;;; Tangling

(defun org-babel-prep-session:lean4 (_session _params)
  "Signal that there is no Lean 4 session to prepare.
A session here is the blocks that have already been run in it, which is
made of whatever the document says.  There is no process to load anything
into ahead of time."
  (user-error "A Lean 4 session has no process to prepare"))

(defun org-babel-lean4-initiate-session (&optional _session _params)
  "Return nil: a Lean 4 session has no process to switch to."
  nil)

(add-to-list 'org-babel-tangle-lang-exts '("lean4" . "lean"))

;;; `lean' as an alias for `lean4'

;; `#+begin_src lean' is what most Lean documents in the wild are written
;; with, and Lean 3 is dead, so treat the two names as the same language.
(defalias 'org-babel-execute:lean #'org-babel-execute:lean4)
(defalias 'org-babel-expand-body:lean #'org-babel-expand-body:lean4)
(defalias 'org-babel-prep-session:lean #'org-babel-prep-session:lean4)
(defvar org-babel-default-header-args:lean
  (copy-sequence org-babel-default-header-args:lean4))
(defconst org-babel-header-args:lean
  (copy-sequence org-babel-header-args:lean4))

(add-to-list 'org-babel-tangle-lang-exts '("lean" . "lean"))

;; `org-src-get-lang-mode' would turn "lean" into the (Lean 3) `lean-mode'.
;; Send both names to `lean4-mode' so that `C-c '' edits either one with
;; Lean 4 highlighting and the "Lean" input method.
(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("lean" . lean4))
  (add-to-list 'org-src-lang-modes '("lean4" . lean4)))

(provide 'ob-lean4)

;;; ob-lean4.el ends here
