;;; lean.el --- Lean 4 for GNU Emacs  -*- lexical-binding: t -*-

(require 'lean4-mode)

;; The toolchain is managed by elan, whose shims live in ~/.elan/bin.
;; `lean4-get-executable' builds its paths as <lean4-rootdir>/bin/<exe>,
;; so pointing it at the elan root is enough for both the language server
;; and ob-lean4 to pick up whatever toolchain the current project pins.
(setq lean4-rootdir (expand-file-name "~/.elan"))

;; Emacs finds executables through `exec-path', subprocesses through PATH;
;; `lake' shells out to `lean' and needs the latter.
(let ((elan-bin (expand-file-name "bin" lean4-rootdir)))
  (when (file-directory-p elan-bin)
    (add-to-list 'exec-path elan-bin)
    (unless (string-match-p (regexp-quote elan-bin) (or (getenv "PATH") ""))
      (setenv "PATH" (concat elan-bin ":" (getenv "PATH"))))))

(defun lean4-maybe-start-lsp ()
  "Start `lsp' in Lean buffers that the language server can actually open.

`lean4-mode' enables `lsp' unconditionally, but the buffer behind an
org-babel block - the one `C-c '' (`org-edit-special') puts you in - is
not visiting a file, so there is no document for the server to open and
no project root to anchor the workspace to.  In those buffers we keep
everything `lean4-mode' gives us offline (syntax table, font locking and
the \"Lean\" input method) and leave the server alone."
  (when (buffer-file-name)
    (lsp)))

(remove-hook 'lean4-mode-hook #'lsp)
(add-hook 'lean4-mode-hook #'lean4-maybe-start-lsp)
(add-hook 'lean4-mode-hook #'company-mode)

;; `lean4-mode' has no indentation function of its own, so the global
;; `newline-and-indent' falls back to `indent-relative' and only copies the
;; previous line's indentation.  Indent one step further after a line that
;; opens a block (`:=', `by', `do', `where', `=>', ...).
(defconst lean4-block-opener-regexp
  (rx (or ":=" "=>" "←" "<-" "λ" "(" "[" "{" "⟨"
          (seq symbol-start
               (or "by" "do" "where" "then" "else" "from" "with" "fun")))
      (* blank) eos)
  "Matches the text of a Lean line after which the next line is indented.")

;; A `·' tactic block runs on for as long as the lines below it are
;; indented past the dot, so the line after `· intro h' belongs under
;; `intro', two columns in.
(defconst lean4-bullet-regexp
  (rx bos (* blank) "·")
  "Matches a Lean line that starts a `·' tactic block.")

(defun lean4-newline-and-indent ()
  "Insert a newline, indenting further after a line that opens a block.
A line that starts with `·' counts as opening one."
  (interactive)
  (let* ((line (buffer-substring-no-properties
                (line-beginning-position) (point)))
         (indent (+ (current-indentation)
                    (if (string-match-p lean4-bullet-regexp line) 2 0)))
         (opens (string-match-p lean4-block-opener-regexp line)))
    (delete-horizontal-space t)
    (newline)
    (indent-to (if opens (+ indent 2) indent))))

;; RET - newline, indented for whatever the previous line opened
(define-key lean4-mode-map (kbd "RET") #'lean4-newline-and-indent)

;; C-x C-e - run the current file through `lean'.  Globally this is
;; `eval-last-sexp', which means nothing in a Lean buffer.  Bound to
;; `lean4-std-exe' rather than `lean4-execute', which prompts for extra
;; `lean' arguments whenever it is called interactively.
(define-key lean4-mode-map (kbd "C-x C-e") #'lean4-std-exe)

;; Lean source is close to unwritable without the \alpha -> α translations,
;; and `set-input-method' in `lean4-mode' only covers Lean buffers.  Typing
;; a block directly in the Org buffer, rather than through `C-c '', needs
;; the input method switched on by hand.
(defun lean4-toggle-input-method ()
  "Toggle the \"Lean\" input method in the current buffer."
  (interactive)
  (require 'lean4-input)
  (if current-input-method
      (deactivate-input-method)
    (set-input-method "Lean")))

;; \. - only `·' (the tactic block dot).  The TeX translations that
;; lean4-input copies add more dots to the same key, so Emacs shows a
;; numbered list instead of inserting it.
(with-eval-after-load 'lean4-input
  (quail-defrule "\\." "·" "Lean"))

;; LaTeX previews in comments.  doc-gen4 renders `$...$' and `$$...$$' in
;; docstrings with KaTeX, so the same fragments are drawn here as images,
;; through Org's renderer and `org-format-latex-options' (org/org-latex.el)
;; so they look like the previews in Org buffers.  Fragments are looked
;; for only inside comments, and a fragment must sit inside one comment:
;; `$$' split over several `--' lines is left alone.
(defconst lean4-latex-fragment-regexp
  (rx (or (seq "$$" (+? anything) "$$")
          (seq "\\[" (+? anything) "\\]")
          (seq "\\(" (+? (not (any "\n"))) "\\)")
          ;; pandoc's rule: no blank right inside the dollars
          (seq "$" (not (any "$ \t\n"))
               (? (*? (not (any "$\n"))) (not (any "$ \t\n")))
               "$")))
  "Matches a LaTeX fragment in a Lean comment.")

(defvar lean4-latex-cache-directory
  (locate-user-emacs-file "lean-latex-cache/")
  "Where rendered fragments are kept, named by a hash of their source.")

(defvar lean4-latex-scale 0.75
  "Size of comment math relative to Org's previews.
Org's :scale suits display math in prose; next to code it looks too big.")

(defun lean4-latex--color (pos)
  "Foreground of the comment text at POS, as a colour Org can parse."
  (let* ((face (get-text-property pos 'face))
         (face (if (consp face) (car face) face)))
    (or (face-foreground (if (facep face) face 'font-lock-comment-face)
                         nil '(font-lock-comment-face default))
        (face-foreground 'default))))

(defun lean4-latex--image (fragment color)
  "Return the SVG file for FRAGMENT drawn in COLOR, rendering on a cache miss."
  (require 'org)
  (let* ((options (copy-sequence org-format-latex-options))
         (options (plist-put options :foreground color))
         (options (plist-put options :scale
                             (* lean4-latex-scale
                                (or (plist-get org-format-latex-options :scale)
                                    1.0))))
         (file (expand-file-name
                (concat (sha1 (format "%s%S%s" fragment options
                                      org-preview-latex-default-process))
                        ".svg")
                lean4-latex-cache-directory)))
    (unless (file-exists-p file)
      (make-directory lean4-latex-cache-directory t)
      ;; The preamble is built from the buffer's #+LATEX_HEADER lines, which
      ;; only means something in an Org buffer.
      (with-temp-buffer
        (delay-mode-hooks (org-mode))
        (org-create-formula-image fragment file options (current-buffer))))
    file))

(defun lean4-latex--overlays (&optional beg end)
  (seq-filter (lambda (ov) (overlay-get ov 'lean4-latex))
              (overlays-in (or beg (point-min)) (or end (point-max)))))

(defun lean4-latex-clear ()
  "Remove the LaTeX previews in the current buffer."
  (mapc #'delete-overlay (lean4-latex--overlays)))

(defun lean4-latex-render ()
  "Show every LaTeX fragment in the comments of this buffer as an image."
  (interactive)
  (lean4-latex-clear)
  ;; The colour is read off the comment's face, so fontify first.
  (font-lock-ensure)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward lean4-latex-fragment-regexp nil t)
      (let* ((beg (match-beginning 0))
             (end (match-end 0))
             (start (nth 8 (save-excursion (syntax-ppss beg)))))
        (if (not (and (nth 4 (save-excursion (syntax-ppss beg)))
                      (eq start (nth 8 (save-excursion (syntax-ppss end))))))
            ;; Not in a comment: retry from the next character, a `$'
            ;; that closed nothing may still open a fragment.
            (goto-char (1+ beg))
          (condition-case err
              (let ((ov (make-overlay beg end nil t nil))
                    (image (create-image
                            (lean4-latex--image (match-string-no-properties 0)
                                                (lean4-latex--color beg))
                            'svg nil :ascent 'center)))
                (overlay-put ov 'lean4-latex image)
                (overlay-put ov 'display image)
                (overlay-put ov 'evaporate t)
                ;; Editing the source drops the image; saving draws it again.
                (overlay-put ov 'modification-hooks
                             (list (lambda (o after &rest _)
                                     (when after (delete-overlay o))))))
            (error (message "LaTeX preview failed at %d: %s"
                            beg (error-message-string err)))))))))

(defun lean4-latex--reveal ()
  "Show the source of the fragment at point, and the image of all others."
  (dolist (ov (lean4-latex--overlays))
    (overlay-put ov 'display
                 (unless (and (>= (point) (overlay-start ov))
                              (<= (point) (overlay-end ov)))
                   (overlay-get ov 'lean4-latex)))))

(define-minor-mode lean4-latex-preview-mode
  "Draw the LaTeX in comments as images.
Moving into a fragment shows its source; saving redraws the buffer."
  :lighter " TeX"
  (if lean4-latex-preview-mode
      (progn
        (lean4-latex-render)
        (add-hook 'post-command-hook #'lean4-latex--reveal nil t)
        (add-hook 'after-save-hook #'lean4-latex-render nil t))
    (remove-hook 'post-command-hook #'lean4-latex--reveal t)
    (remove-hook 'after-save-hook #'lean4-latex-render t)
    (lean4-latex-clear)))

;; C-c C-t - toggle the LaTeX previews.  Org's `C-c C-x C-l' is out of
;; reach: `C-c C-x' alone already runs the file.
(define-key lean4-mode-map (kbd "C-c C-t") #'lean4-latex-preview-mode)

(with-eval-after-load 'org
  ;; C-c C-x i - switch the Lean input method on and off
  (define-key org-mode-map (kbd "C-c C-x i") #'lean4-toggle-input-method)
  ;; C-c C-x k - how do I type the symbol at point?  This is `C-c C-k' in a
  ;; Lean buffer, and it needs the input method above to be active.
  (define-key org-mode-map (kbd "C-c C-x k") #'quail-show-key))
