;;; org-markdown.el --- Auxiliary functions to export org to markdown  -*- lexical-binding: t -*-

(require 'ox-gfm)

(defun export-org-directory-to-gfm (src-dir out-dir)
  "Export all .org files in SRC-DIR to .md in OUT-DIR, preserving relative structure."
  (let ((files (directory-files-recursively src-dir "\\.org$")))
    (dolist (org-file files)
      (let* ((rel-path (file-relative-name org-file src-dir))
             (md-path (concat (file-name-sans-extension
                               (expand-file-name rel-path out-dir)) ".md")))
        (make-directory (file-name-directory md-path) t)
        (with-current-buffer (find-file-noselect org-file)
          (let ((org-export-with-toc nil))
            (let ((output (org-gfm-export-to-markdown)))
              (copy-file output md-path t)
              (delete-file output)))
          (kill-buffer))))))
