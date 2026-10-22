;;; ocaml.el --- OCaml for GNU Emacs  -*- lexical-binding: t -*-

;; tuareg is the OCaml major mode (.ml, .mli, .mly, .mll, .opam ...) and
;; dune-mode covers dune, dune-project and dune-workspace files.  Both
;; register their files through autoloads, so nothing is loaded until the
;; first OCaml buffer is opened.
;;
;; Inside a tuareg buffer:
;;
;;   C-c C-s - start the OCaml toplevel (REPL)
;;   C-c C-e - send the phrase at point to the toplevel
;;   C-c C-b - send the whole buffer to the toplevel
;;   C-c C-c - compile, `dune build' by default (see below)
;;   C-c C-a - switch between .ml and .mli

;; opam installs ocaml-lsp-server, ocamlformat, utop ... into the switch's
;; bin/; add it if there is one.  The system ocaml and dune in /usr/bin
;; work without it.
(let ((opam-bin (expand-file-name "~/.opam/default/bin")))
  (when (file-directory-p opam-bin)
    (add-to-list 'exec-path opam-bin)
    (unless (string-match-p (regexp-quote opam-bin) (or (getenv "PATH") ""))
      (setenv "PATH" (concat opam-bin ":" (getenv "PATH"))))))

;; Use utop as the toplevel when it is installed: completion, history and
;; colours, instead of the plain `ocaml' prompt.
(with-eval-after-load 'tuareg
  (when (executable-find "utop")
    (setq tuareg-interactive-program "utop -emacs")))

;; Show `fun' as λ, `->' as →, `<=' as ≤ and so on.  Only the display
;; changes, the file keeps the ASCII.
(setq tuareg-prettify-symbols-full t)
(add-hook 'tuareg-mode-hook #'prettify-symbols-mode)

;; Build with dune from the project root rather than `make'
(add-hook 'tuareg-mode-hook
          (lambda ()
            (setq-local compile-command "dune build")))

(defun ocaml-maybe-start-lsp ()
  "Start `lsp' in OCaml buffers if ocaml-lsp-server is installed.
The check runs per buffer, so `opam install ocaml-lsp-server' takes
effect without restarting Emacs.  With the server we get types on hover,
go to definition, completion, diagnostics and - when the project has an
.ocamlformat file - formatting on save."
  (when (and (buffer-file-name)
             (executable-find "ocamllsp"))
    (lsp-deferred)
    (add-hook 'before-save-hook #'ocaml-format-if-configured nil t)))

(defun ocaml-format-if-configured ()
  "Format the buffer with ocamlformat, if the project asks for it.
ocamlformat refuses to run without an .ocamlformat file, and reformatting
code in projects that do not use it would only produce noisy diffs."
  (when (and (bound-and-true-p lsp-mode)
             (locate-dominating-file default-directory ".ocamlformat"))
    (lsp-format-buffer)))

(add-hook 'tuareg-mode-hook #'ocaml-maybe-start-lsp)
(add-hook 'tuareg-mode-hook #'company-mode)
