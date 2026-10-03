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

(with-eval-after-load 'org
  ;; C-c C-x i - switch the Lean input method on and off
  (define-key org-mode-map (kbd "C-c C-x i") #'lean4-toggle-input-method)
  ;; C-c C-x k - how do I type the symbol at point?  This is `C-c C-k' in a
  ;; Lean buffer, and it needs the input method above to be active.
  (define-key org-mode-map (kbd "C-c C-x k") #'quail-show-key))
