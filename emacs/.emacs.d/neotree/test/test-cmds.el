;;; test-cmds.el --- test cases

;; Copyright (C) 2014 jaypei

;; Author: jaypei <jaypei97159@gmail.com>
;; URL: https://github.com/jaypei/emacs-neotree

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

;;; Code:

(require 'neotree)
(require 'neotree-test)

(defmacro neo-tests--with-temp-dir (&rest body)
  (declare (indent 0) (debug t))
  (let ((dir (gensym)))
    `(let ((,dir (file-name-as-directory (make-temp-file "dir" t))))
       (unwind-protect
           (let ((default-directory ,dir)) ,@body)
         (delete-directory ,dir t)))))

(defadvice window-at (around neo-test-neotree-startup activate)
  ad-do-it
  (setq ad-return-value (selected-window))
  ad-return-value)

(ert-deftest neo-test-neotree-startup ()
  (neotree)
  (should (neo-global--window-exists-p)))

(ert-deftest neo-test-neotree-toggle ()
  (neotree-toggle)
  (should (neo-global--window-exists-p))
  (neotree-show)
  (should (neo-global--window-exists-p))
  (neotree-toggle)
  (should (not (neo-global--window-exists-p)))
  (neotree-hide)
  (should (not (neo-global--window-exists-p)))
  (neotree-show)
  (should (neo-global--window-exists-p)))

(ert-deftest neo-test-neotree-dir ()
  (neo-test--with-temp-dir
   (neotree-dir temp-cwd)
   (neo-global--with-buffer
    (should (string-equal neo-buffer--start-node temp-cwd)))
   (neotree-toggle)
   (neo-global--with-buffer
    (should (string-equal neo-buffer--start-node temp-cwd)))
   (neotree-toggle)
   (neo-global--with-buffer
    (should (string-equal neo-buffer--start-node temp-cwd)))))

(ert-deftest neo-test-save-current-pos ()
  (neotree)
  (neo-global--select-window)
  (beginning-of-line)
  (condition-case err
      (while t
        (next-line)
        (neo-buffer--save-cursor-pos)
        (let ((current-file-path (neo-buffer--get-filename-current-line))
              (current-line-number (line-number-at-pos)))
          (should (eq (car neo-buffer--cursor-pos) current-file-path))
          (should (eq (cdr neo-buffer--cursor-pos) current-line-number))))
    (error
     (should (equal err '(end-of-buffer)))))
  (neo-buffer--save-cursor-pos "/tmp/nbs" 192)
  (should (equal neo-buffer--cursor-pos (cons "/tmp/nbs" 192))))

;;; test-cmds.el ends here
