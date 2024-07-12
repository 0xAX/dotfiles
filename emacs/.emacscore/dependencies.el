;;; dependencies.el --- Package management  -*- lexical-binding: t -*-

(require 'json)
(require 'subr-x)

(setq packages
      '((:name "bison-mode"
         :repo "https://github.com/Wilfred/bison-mode.git"
         :revision "4f2e20394a475931409618c1635e9c9f1cf07d9c")
        (:name "bnf-mode"
         :repo "https://github.com/sergeyklay/bnf-mode.git"
         :revision "1a7e177c282b8e07a2c33bd89232464b347dfc17")
        (:name "company"
         :repo "https://github.com/company-mode/company-mode.git"
         :revision "695b94b86ee5b24641724260b296bdc247690f4a")
        (:name "company-c-headers"
         :repo "https://github.com/randomphrase/company-c-headers.git"
         :revision "9d384571b1190e99d0a789e5296176d69a3d0771")
        (:name "dash"
         :repo "https://github.com/magnars/dash.el.git"
         :revision "1de9dcb83eacfb162b6d9a118a4770b1281bcd84")
        (:name "elixir-mode"
         :repo "https://github.com/elixir-editors/emacs-elixir.git"
         :revision "00d6580a040a750e019218f9392cf9a4c2dac23a")
        (:name "erlang-mode"
         :repo "https://github.com/erlang/otp"
         :revision "945c940f6bc6c0bcb026cdc6ae8f3ce358e859bb"
         :lisp-dir "lib/tools/emacs")
        (:name "f"
         :repo "https://github.com/rejeep/f.el.git"
         :revision "1e7020dc0d4c52d3da9bd610d431cab13aa02d8c")
        (:name "flycheck"
         :repo "https://github.com/flycheck/flycheck.git"
         :revision "10430dee428f7bab176743097d996182fac29daa")
        (:name "go-mode"
         :repo "https://github.com/dominikh/go-mode.el.git"
         :revision "6f4ff9ef874d151ed8d297a80f1bf27db5d9dbf0")
        (:name "ivy-posframe"
         :repo "https://github.com/tumashu/ivy-posframe.git"
         :revision "533a8e368fcabfd534761a5c685ce713376fa594")
        (:name "lsp-mode"
         :repo "https://github.com/emacs-lsp/lsp-mode.git"
         :revision "4b87e72418017faec08a09f2b0933e6fec2e421f")
        (:name "lsp-treemacs"
         :repo "https://github.com/emacs-lsp/lsp-treemacs.git"
         :revision "1d43e9e0307f84496a4a7ddf9dba481000391dbd")
        (:name "lsp-ui"
         :repo "https://github.com/emacs-lsp/lsp-ui.git"
         :revision "00f1fecdfb41c30428734cf27e492f26f46627fb")
        (:name "magit"
         :repo "https://github.com/magit/magit.git"
         :revision "f5ddce8c8459ffcbb2bd3ae759259d6b627c69cd"
         :lisp-dir "lisp")
        (:name "markdown-mode"
         :repo "https://github.com/jrblevin/markdown-mode.git"
         :revision "0cdebc833ed9b98baf9f260ed12b1e36b0ca0e89")
        (:name "meson-mode"
         :repo "https://github.com/wentasah/meson-mode"
         :revision "c8f4fbf075bb5db2bc0872afe02af2edac075e4e")
        (:name "rust-mode"
         :repo "https://github.com/rust-lang/rust-mode.git"
         :revision "d00d83d3a207a5b7c2994392b2781f627e3159ce")
        (:name "rustic"
         :repo "https://github.com/brotzeit/rustic.git"
         :revision "39423d1cf4fa054c36bf9577356451f4c06ee148")
	(:name "s.el"
         :repo "https://github.com/magnars/s.el.git"
         :revision "dda84d38fffdaf0c9b12837b504b402af910d01d")
        (:name "solarized-emacs"
         :repo "https://github.com/bbatsov/solarized-emacs.git"
         :revision "922b5956a9e2e474f1595bad7b2b35f148b4df3f")
        (:name "slime"
         :repo "https://github.com/slime/slime.git"
         :revision "5ced74ab35d91f6be7d8fa10e1098aaae6b749e3")
        (:name "slime-company"
         :repo "https://github.com/anwyn/slime-company.git"
         :revision "f20ecc4104d4c35052696e7e760109fb02060e72")
        (:name "swiper"
         :repo "https://github.com/abo-abo/swiper.git"
         :revision "8133016ab1b37da233e6daaab471e40abf0f7ba9")
        (:name "tabbar"
         :repo "https://github.com/dholm/tabbar.git"
         :revision "82bbda31cbe8ef367dd6501c3aa14b7f2c835910")
        (:name "yaml-mode"
         :repo "https://github.com/yoshiki/yaml-mode"
         :revision "7b5ce294fb15c2c8926fa476d7218aa415550a2a")
        (:name "yasnippet"
         :repo "https://github.com/joaotavora/yasnippet"
         :revision "eb5ba2664c3a68ae4a53bb38b85418dd131b208f")
        (:name "yasnippet-snippets"
         :repo "https://github.com/AndreaCrotti/yasnippet-snippets"
         :revision "6fafad13bb4689600285d9e38c61958dd63c356d")        
        (:name "vterm"
         :repo "https://github.com/akermu/emacs-libvterm.git"
         :revision "df057b1af2bb89a1deb288086f13be296af42090")
        (:name "with-editor"
         :repo "https://github.com/magit/with-editor.git"
         :revision "1b4526453ef6bdee30635f469aa26085c02b1ac1"
         :lisp-dir "lisp")))

(defconst lock-file
  (concat user-emacs-configuration-directory "/dependencies.lock")
  "Path to the dependencies lock file.")

(defun find-package-in-lock-file (package-name)
  "Find a package with the given PACKAGE-NAME in the lock-file and fetch all
the information about it."
  (let* ((json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (json (json-read-file lock-file)))
    (catch 'stop-mapcar
      (mapcar
       (lambda (dependency-name)
         (let ((repo (gethash "repo" (gethash dependency-name json)))
               (revision (gethash "revision" (gethash dependency-name json))))
           (when (string= package-name dependency-name)
             (throw 'stop-mapcar (list repo revision)))))
            (hash-table-keys json)))))

(defun maybe-update-dependency (name repo revision lisp-dir locked-repo locked-revision)
  "Check is dependency should be updated and update if it is needed."
  (catch 'exit
    (when (not (string= repo locked-repo))
      (progn
        (install-package name repo revision lisp-dir)
        (puthash name (list :repo repo :revision revision :lisp-dir nil) lock-table)
        (throw 'exit 'stop)))
    (when (not (string= revision locked-revision))
      (progn
        (install-package name repo revision lisp-dir)
        (puthash name (list :repo repo :revision revision :lisp-dir nil) lock-table)
        (throw 'exit 'stop)))
    (puthash name (list :repo repo :revision revision) lock-table)))

(defun update-dependency (name repo revision lisp-dir)
  "Update the dependency with the given NAME, REPO and REVISION."
  (install-package name repo revision lisp-dir)
  (puthash name (list :repo repo :revision revision :lisp-dir nil) lock-table))

(defun install-package (name repo revision lisp-dir)
  "Install the dependency with the given NAME, from the given REPO with the
given REVISION."
  (if lisp-dir
      (let ((name-sym (intern name)))
      (package-vc-install `(,name-sym :url ,repo :lisp-dir ,lisp-dir) revision))
    (package-vc-install repo revision)))

(defun update-packages ()
  "Update packages specified in PACKAGES variable from this module."
  (when (not (file-exists-p lock-file))
    (write-region "{}" nil lock-file))
  (setq lock-table (make-hash-table :test 'equal))
  (dolist (package packages)
    (let* ((name (plist-get package :name))
           (repo (plist-get package :repo))
           (revision (plist-get package :revision))
           (lisp-dir (plist-get package :lisp-dir))
           (locked-deps (find-package-in-lock-file name)))
      (if (listp locked-deps)
          (maybe-update-dependency name repo revision lisp-dir (car locked-deps) (car (cdr locked-deps)))
        (update-dependency name repo revision lisp-dir))))
  (with-temp-buffer
    (insert (json-encode lock-table))
    (write-region (point-min) (point-max) lock-file)))
