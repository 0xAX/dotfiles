;;; .emacs --- My org-mode configuration  -*- lexical-binding: t -*-

(defun org-eval-code-block ()
  "Evaluate the org-babel code block at point, dropping the buffers and
windows that appear during evaluation.

Outside a code block this falls back to `org-open-at-point', so C-c C-o
still follows links.

Evaluation goes through `org-babel-execute-src-block' rather than
`org-open-at-point'.  The latter runs the block and then jumps to its
results, which fails with `integer-or-marker-p, nil' for a block that
produces no results at all - a Lean block that only makes definitions,
or anything with `:results none' - because there is nowhere to jump."
  (interactive)
  (if (not (org-in-src-block-p))
      (org-open-at-point)
    ;; Strip the prefixes first: they are not part of any result, so
    ;; clearing the results would strand them.
    (org-babel-remove-results-prefixes)
    (org-babel-remove-result-one-or-many t)
    (org-babel-execute-src-block)
    (when (get-buffer "*Org Babel Results*")
      (kill-buffer "*Org Babel Results*"))
    (when (get-buffer "*Org-Babel Error Output*")
      (switch-to-buffer "*Org-Babel Error Output*" 'norecord t))))

;; Org ships no backend for Lean, so load ours before the languages are
;; enabled below: `org-babel-do-load-languages' only does `require', and
;; ~/.emacscore/lisp is not on `load-path'.
(load "~/.emacscore/lisp/ob-lean4.el")

;; List of langauges supported by babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)
   (C . t)
   (emacs-lisp . t)
   (lean4 . t)
   (lisp . t)
   (julia . t)
   (plantuml . t)
   (latex . t)
   (octave . t)
   (julia . t)
   (perl . t)
   (scheme . t)
   (shell . t)
   (sql . t)))

;; Do not execute code on C-c C-c by default as it will be re-binded
(setq org-babel-no-eval-on-ctrl-c-ctrl-c t)

;; If we are in i3 environment add special hooks for code block
;; execution to avoid dances with i3 modes
(when (string= *i3* "true")
  (progn
    (add-hook 'org-babel-execute-src-block-hook
              #'(lambda () (org-execute-code-block)))
    (add-hook 'org-babel-after-execute-hook
              #'(lambda () (shell-command-to-string "i3-msg mode passthrough")))))

;; Remove confirmation for code execution,
(setq org-confirm-babel-evaluate nil)
(setq org-export-use-babel nil)
(setq org-adapt-indentation nil)

;; Default flags passed to each C code block
(defvar org-babel-default-header-args:C
  '((:flags . "-Wall -Wextra -Werror -Wstrict-prototypes  -Wcast-qual -Wconversion -Wpedantic -std=c17")))

;; Add prefix before the "RESULTS" of the org-babel code execution
(defvar org-babel-results-prefix "The result:"
  "Text put in front of the `#+RESULTS:' of an executed code block.

Org locates an unnamed block's results by looking at the element
directly below the block, tolerating only affiliated keywords in
between, so this paragraph detaches a result from the block that
produced it while it is there.  `org-eval-code-block' works around that
by calling `org-babel-remove-results-prefixes' before it touches any
result, which puts every result back within reach of
`org-babel-remove-result'.

The consequence is that the prefixes are only consistent inside that
command.  Running a block with the stock `C-c C-v C-e' instead appends a
second copy of its results rather than replacing the first, because the
prefix is still in the way.")

;; `defvar' only initialises an unbound variable, so on its own the value
;; above would never reach a session that already loaded this file.  Set
;; it outright, so that re-loading org-babel.el actually takes effect.
(setq org-babel-results-prefix "The result:")

(defun org-babel-remove-results-prefixes ()
  "Delete every `org-babel-results-prefix' paragraph in the buffer.

Two reasons to do this before clearing results.  It reattaches each
result to its block, without which `org-babel-remove-result' cannot find
it; and it drops the prefixes of results that are about to go away,
which `org-babel-remove-result' would otherwise leave stranded."
  (save-excursion
    (goto-char (point-min))
    ;; The optional `#+CAPTION: ' also sweeps up the affiliated-keyword
    ;; spelling this prefix briefly used, so buffers that picked one up
    ;; clean themselves on the next run.
    (let ((regexp (concat "^[ \t]*\\(?:#\\+CAPTION: \\)?"
                          (regexp-quote org-babel-results-prefix)
                          "[ \t]*\n\\(?:[ \t]*\n\\)?")))
      (while (re-search-forward regexp nil t)
        (replace-match "")))))

(defun add-prefix-before-results ()
  "Put `org-babel-results-prefix' before the #+RESULTS: line of the
code block that was just executed.

Does nothing if the prefix is already there, so re-running a block does
not stack up copies of it."
  (save-excursion
    (let ((result-pos (org-babel-where-is-src-block-result)))
      (when result-pos
        (goto-char result-pos)
        (beginning-of-line)
        ;; Look past the blank line the prefix leaves behind it.
        (unless (save-excursion
                  (skip-chars-backward " \t\n")
                  (forward-line 0)
                  (looking-at-p (concat "[ \t]*"
                                        (regexp-quote org-babel-results-prefix)
                                        "[ \t]*$")))
          (insert org-babel-results-prefix "\n\n"))))))

(add-hook 'org-babel-after-execute-hook 'add-prefix-before-results)

;; Add env variable to determine that we are in babel code block
(with-eval-after-load 'ob-python
  (add-to-list 'org-babel-default-header-args:python
               '(:prologue . "import os; os.environ['ORG_BABEL']='1'")))
