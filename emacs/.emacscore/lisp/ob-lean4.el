;;; ob-lean4.el --- Org-babel support for Lean 4  -*- lexical-binding: t -*-

;;; Commentary:

;; Org does not ship an `ob-' backend for Lean, so this file provides one
;; for Lean 4.  A block is compiled by writing it to a temporary file and
;; running `lean --json' over it; the JSON messages Lean emits (the results
;; of `#eval' and `#check', plus warnings and errors) become the result of
;; the block.
;;
;; Lean 4 has no REPL, so there is nothing to hold a session: every block
;; is a self-contained file.  Anything a block needs must come from an
;; `import' or be written in the block itself.
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
;;               many blocks, or from a `#+PROPERTY:' line; writing the
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

;;; Locating the toolchain

(defun org-babel-lean4--executable (name)
  "Return the full path of the Lean toolchain executable NAME.
Prefers whatever `lean4-mode' is configured to use, so that a block and
the language server always run the same toolchain."
  (cond
   ;; `lean4-get-executable' honours `lean4-rootdir', which is where the
   ;; elan shims live; see ~/.emacscore/dev/lean.el.
   ((fboundp 'lean4-get-executable) (lean4-get-executable name))
   ((and (boundp 'lean4-rootdir) (stringp lean4-rootdir))
    (expand-file-name name (expand-file-name "bin" lean4-rootdir)))
   ((executable-find name))
   (t (error "ob-lean4: cannot find `%s'; set `lean4-rootdir'" name))))

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
blank lines and comments among them; REST is everything below.  Lean
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

(defun org-babel-lean4--generated-lines (params)
  "Return the lines ob-lean4 generates for PARAMS.
They are slotted between the block's own `import' lines and the rest of
its body: after the imports, because Lean wants those first, and before
the body, because the body is what uses them."
  (append (org-babel-lean4--import-lines params)
          (let ((prologue (cdr (assq :prologue params))))
            (when (org-string-nw-p prologue)
              (split-string prologue "\n")))
          (org-babel-lean4--var-lines params)))

(defun org-babel-expand-body:lean4 (body params)
  "Expand BODY into the Lean 4 file that will be compiled, per PARAMS."
  (pcase-let ((`(,head . ,rest) (org-babel-lean4--split-imports body))
              (epilogue (cdr (assq :epilogue params))))
    (mapconcat #'identity
               (append (when (org-string-nw-p head) (list head))
                       (org-babel-lean4--generated-lines params)
                       (list rest)
                       (when (org-string-nw-p epilogue) (list epilogue)))
               "\n")))

(defun org-babel-lean4--body-line (line head generated)
  "Map LINE in the generated file back onto the block body.

The file is HEAD lines of the block's own imports, then GENERATED lines
this file produced, then the rest of the block.  A line from the
generated middle belongs to none of the block, and is reported as 0 so
that it can be labelled as coming from the preamble."
  (cond
   ((<= line head) line)
   ((<= line (+ head generated)) 0)
   (t (- line generated))))

;;; Running Lean

(defun org-babel-lean4--run (file params)
  "Run Lean over FILE according to PARAMS.
Return a cons cell (EXIT-CODE . OUTPUT); Lean writes its diagnostics to
standard output, and standard error is merged into it so that a crash or
a Lake failure is not silently dropped."
  (let* ((lake (equal "yes" (cdr (assq :lake params))))
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

(defun org-babel-lean4--parse (output head generated)
  "Parse the `lean --json' OUTPUT into a list of messages.
Each message is a list (SEVERITY LINE COLUMN TEXT), with LINE mapped
back onto the block body by `org-babel-lean4--body-line' given the HEAD
and GENERATED line counts.  A line that is not valid JSON - Lake noise, a
panic - is kept verbatim with severity `raw', so nothing Lean printed can
go missing."
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
                     (org-babel-lean4--body-line
                      (or (alist-get 'line pos) 0) head generated)
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
  (let ((filter (or (cdr (assq :messages params)) "all"))
        (positions (or (cdr (assq :positions params)) "diag"))
        lines)
    (pcase-dolist (`(,severity ,line ,column ,text) messages)
      (when (org-babel-lean4--keep-p severity filter)
        (push (if (org-babel-lean4--prefix-p severity positions)
                  (format "%s %s: %s"
                          ;; A non-positive line means the message came from
                          ;; what `:imports', `:var' or `:prologue' put in
                          ;; front of the block, not from the block itself.
                          (if (> line 0)
                              (format "L%d:%d" line column)
                            "preamble")
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
  (let ((session (cdr (assq :session params))))
    (when (and session (not (equal session "none")))
      (user-error
       "ob-lean4: Lean 4 has no REPL, so `:session' is not supported")))
  (let* ((head (car (org-babel-lean4--split-imports body)))
         (head-lines (if (org-string-nw-p head)
                         (length (split-string head "\n"))
                       0))
         (generated-lines (length (org-babel-lean4--generated-lines params)))
         (file (org-babel-temp-file "lean4-" ".lean")))
    (with-temp-file file
      (insert (org-babel-expand-body:lean4 body params)))
    (pcase-let* ((`(,exit . ,output) (org-babel-lean4--run file params))
                 (rendered (org-babel-lean4--render
                            (org-babel-lean4--parse
                             output head-lines generated-lines)
                            params)))
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

;;; Sessions and tangling

(defun org-babel-prep-session:lean4 (_session _params)
  "Signal that Lean 4 src blocks cannot be prepared in a session."
  (user-error "ob-lean4: Lean 4 has no REPL, so `:session' is not supported"))

(defun org-babel-lean4-initiate-session (&optional _session _params)
  "Return nil: Lean 4 has no REPL to start a session in."
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

;; `org-src-get-lang-mode' would turn "lean" into the (Lean 3) `lean-mode';
;; send both names to `lean4-mode' so that `C-c '' edits either one with
;; Lean 4 highlighting and the "Lean" input method.
(with-eval-after-load 'org-src
  (add-to-list 'org-src-lang-modes '("lean" . lean4))
  (add-to-list 'org-src-lang-modes '("lean4" . lean4)))

(provide 'ob-lean4)

;;; ob-lean4.el ends here
