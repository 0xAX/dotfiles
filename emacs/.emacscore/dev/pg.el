;;; pg.el --- PG configuration -*- lexical-binding: t -*-

(defconst +pg-dir+ "~/.emacs.d/PG/generic/proof-site")

(when (file-directory-p +pg-dir+)
  (load "~/.emacs.d/PG/generic/proof-site"))
