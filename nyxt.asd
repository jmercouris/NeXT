;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

#+sbcl
(sb-ext:assert-version->= 1 5 0)

(defvar *quicklisp-dir* (or (uiop:getenv "NYXT_QUICKLISP_DIR")
                           "_build/quicklisp-client"))
(defvar *submodules-dir* (or (uiop:getenv "NYXT_SUBMODULES_DIR")
                            "_build/submodules"))

(defvar *prefix* (format nil "~a/~a"
                         (or (uiop:getenv "DESTDIR") "")
                         (or (uiop:getenv "PREFIX")
                             "/usr/local")))
(defvar *datadir* (or (uiop:getenv "DATADIR")
                      (format nil "~a/share" *prefix*)))
(defvar *bindir* (or (uiop:getenv "BINDIR")
                     (format nil "~a/bin" *prefix*)))

(defvar *nyxt-renderer* (or (uiop:getenv "NYXT_RENDERER")
                            "gobject/gtk"))

(defsystem "nyxt"
  :version "2" ; Pre-release 6
  :author "Atlas Engineer LLC"
  :homepage "https://nyxt.atlas.engineer"
  :description "Extensible web-browser in Common Lisp"
  :license "BSD 3-Clause"
  :serial t
  :depends-on (alexandria
               bordeaux-threads
               calispel
               cl-css
               cl-html-diff
               cl-json
               cl-markup
               cl-ppcre
               cl-ppcre-unicode
               cl-prevalence
               closer-mop
               cl-containers
               moptilities
               dexador
               enchant
               file-attributes
               iolib
               local-time
               log4cl
               mk-string-metrics
               #-sbcl
               osicat
               parenscript
               quri
               serapeum
               str
               plump
               swank
               trivia
               trivial-clipboard
               trivial-features
               trivial-package-local-nicknames
               trivial-types
               unix-opts
               ;; Local systems:
               nyxt/user-interface
               nyxt/text-buffer
               nyxt/analysis
               nyxt/download-manager
               nyxt/history-tree
               nyxt/password-manager
               nyxt/keymap
               nyxt/class-star
               nyxt/ospm
               nyxt/prompter)
  :pathname "source/"
  :components ((:file "package")
               ;; Independent utilities
               (:file "time")
               (:file "types")
               (:file "conditions")
               (:file "user-interface")
               ;; Core functionality
               (:file "global")
               (:file "concurrency")
               (:file "data-storage")
               (:file "configuration")
               (:file "command")
               (:file "parenscript-macro")
               (:file "renderer-script")
               (:file "buffer")
               (:file "window")
               (:file "mode")
               (:file "search-engine")
               (:file "urls")
               (:file "browser")
               (:file "notification")
               (:file "clipboard")
               (:file "message")
               (:file "input")
               (:file "prompt-buffer")
               (:file "prompt-buffer-mode")
               (:file "command-commands")
               (:file "recent-buffers")
               (:file "password")
               (:file "bookmark")
               (:file "history")
               (:file "autofill")
               (:file "auto-mode")
               (:file "external-editor")
               (:file "file-manager")
               #+quicklisp
               (:file "lisp-system")
               ;; Core Modes
               (:file "editor-mode")
               (:file "plaintext-editor-mode")
               (:file "buffer-listing-mode")
               (:file "base-mode")
               (:file "repl-mode")
               (:file "help-mode")
               (:file "message-mode")
               (:file "application-mode")
               (:file "history-tree-mode")
               (:file "list-history-mode")
               (:file "web-mode")
               (:file "reading-line-mode")
               (:file "style-mode")
               (:file "certificate-exception-mode")
               (:file "emacs-mode")
               (:file "vi-mode")
               (:file "blocker-mode")
               (:file "proxy-mode")
               (:file "noimage-mode")
               (:file "nosound-mode")
               (:file "noscript-mode")
               (:file "nowebgl-mode")
               (:file "download-mode")
               (:file "force-https-mode")
               (:file "reduce-tracking-mode")
               (:file "os-package-manager-mode")
               (:file "visual-mode")
               (:file "watch-mode")
               (:file "diff-mode")
               ;; Web-mode commands
               (:file "bookmarklets")
               (:file "input-edit")
               (:file "element-hint")
               (:file "element-hint-mode")
               (:file "jump-heading")
               (:file "scroll")
               (:file "search-buffer")
               (:file "spell-check")
               (:file "zoom")
               ;; Needs web-mode
               (:file "help")
               (:file "status")
               ;; Depends on everything else:
               (:file "about")
               (:file "start")
               (:file "tutorial")
               (:file "manual"))
  :in-order-to ((test-op (test-op "nyxt/tests")
                         (test-op "nyxt/download-manager/tests")
                         (test-op "nyxt/history-tree/tests")
                         (test-op "nyxt/keymap/tests")
                         (test-op "nyxt/class-star/tests")
                         (test-op "nyxt/ospm/tests")
                         (test-op "nyxt/prompter/tests"))))

(defun nyxt-run-test (c path &key network-needed-p)
  (and (or (not network-needed-p)
           (not (uiop:getenv "NYXT_TESTS_NO_NETWORK")))
       (not (funcall (read-from-string "prove:run")
                     (system-relative-pathname c path)))
       (uiop:getenv "NYXT_TESTS_ERROR_ON_FAIL")
       (uiop:quit 18)))

;; TODO: Test that Nyxt starts and that --help, --version work.
(defsystem "nyxt/tests"
  :depends-on (nyxt prove)
  :components ((:file "tests/package"))
  :perform (test-op (op c)
                    (nyxt-run-test c "tests/offline/")
                    (nyxt-run-test c "tests/online/" :network-needed-p t)))

(defun print-benchmark (benchmark-results)
  (labels ((rat->float (num)
             (if (integerp num) num (float num)))
           (print-times (entry)
             (let ((title (first entry))
                   (attr (rest entry)))
               (unless (or (member (symbol-name title) '("RUN-TIME" "SYSTEM-RUN-TIME")) ; Not so interesting.
                           (and (member (symbol-name title) '("PAGE-FAULTS" "EVAL-CALLS")
                                        :test #'string=)
                                (zerop (getf attr :average))))
                 (format t " ~a: ~,9t~a" (string-downcase title) (rat->float (getf attr :average)))
                 (format t "~32,8t[~a, ~a]"
                         (rat->float (getf attr :minimum))
                         (rat->float (getf attr :maximum)))
                 (format t "~56,8t(median ~a, deviation ~a, total ~a)"
                         (rat->float (getf attr :median))
                         (rat->float (getf attr :deviation))
                         (rat->float (getf attr :total)))
                 (format t "~%")))))
    (dolist (mark benchmark-results)
      (format t "~a (~a sample~:p):~%" (first mark)
              (getf (rest (second mark)) :samples))
      (mapc #'print-times (rest mark)))))

(defsystem "nyxt/benchmark"
  :depends-on (alexandria
               nyxt
               trivial-benchmark)
  :pathname "tests/benchmarks"
  :components ((:file "../benchmark-package")
               (:file "prompter"))
  :perform (test-op (op c)
                    (let ((results
                            (funcall (read-from-string "alexandria:hash-table-alist")
                                     (funcall (read-from-string "benchmark:run-package-benchmarks")
                                              :package :nyxt/benchmark
                                              :verbose t))))
                      (print-benchmark results))))

(defsystem "nyxt/submodules"
  :perform (compile-op (o c)
                       (uiop:run-program `("git"
                                           "-C" ,(namestring (system-relative-pathname c ""))
                                           ;; TODO: Pass --force to ensure submodules are checked out?
                                           "submodule" "update" "--init")
                                         :ignore-error-status t)))

(defsystem "nyxt/quicklisp"
  :depends-on (nyxt/submodules)
  :perform (compile-op (o c)
                       (load (system-relative-pathname
                              c
                              (format nil "~a/setup.lisp" *quicklisp-dir*)))
                       (setf (symbol-value (read-from-string "ql:*local-project-directories*"))
                             (cons
                              (uiop:truenamize (uiop:ensure-directory-pathname *submodules-dir*))
                              (symbol-value (read-from-string "ql:*local-project-directories*"))))
                       (funcall (read-from-string "ql:update-dist")
                                "quicklisp" :prompt nil)))

(defsystem "nyxt/clean-fasls"
  :depends-on (swank)
  :perform (compile-op (o c)
                       (load (merge-pathnames
                              "contrib/swank-asdf.lisp"
                              (symbol-value
                               (read-from-string "swank-loader:*source-directory*"))))
                       (funcall (read-from-string "swank:delete-system-fasls") "nyxt")))

;; We use a temporary "version" file to generate the final nyxt.desktop with the
;; right version number.  Since "version" is a file target, third-party
;; packaging systems can choose to generate "version" in advance before calling
;; "make install-assets", so that they won't need to rely on Quicklisp.
(defsystem "nyxt/version"
  :depends-on (nyxt)
  :output-files (compile-op (o c)
                            (values (list (system-relative-pathname c "version"))
                                    t))
  :perform (compile-op (o c)
                       (with-open-file (out (output-file o c)
                                            :direction :output
                                            :if-exists :supersede)
                         (princ (symbol-value (read-from-string "nyxt:+version+"))
                                out))))

(defsystem "nyxt/documentation"         ; TODO: Only rebuild if input changed.
  :depends-on (nyxt)
  :output-files (compile-op (o c)
                            (values (list (system-relative-pathname c "manual.html"))
                                    t))
  :perform (compile-op (o c)
                       (with-open-file (out (output-file o c)
                                            :direction :output
                                            :if-exists :supersede)
                         (write-string (funcall (read-from-string "nyxt::manual-content"
                                                                  (find-package 'nyxt)))
                                       out))
                       (format *error-output* "Manual dumped to ~s.~&" (output-file o c))))

(defsystem "nyxt/gtk"
  :depends-on (nyxt
               cl-cffi-gtk
               cl-webkit2)
  :pathname "source/"
  :components ((:file "renderer-gtk")))

(defsystem "nyxt/gobject/gtk"
  :depends-on (nyxt/gtk
               cl-gobject-introspection
               bordeaux-threads)
  :pathname "source/"
  :components ((:file "renderer-gobject-gtk"))
  :in-order-to ((test-op (test-op "nyxt/gobject/gtk/tests"))))

(defsystem "nyxt/gobject/gtk/tests"
  :depends-on (nyxt/gobject/gtk prove)
  :components ((:file "tests/renderer-package"))
  :perform (test-op (op c)
                    (nyxt-run-test c "tests/renderer-offline/")
                    (nyxt-run-test c "tests/renderer-online/" :network-needed-p t)))

(defsystem "nyxt/qt"
  :depends-on (nyxt
               cl-webengine
               trivial-main-thread)
  :pathname "source/"
  :components ((:file "renderer-qt")))

;; We should not set the build-pathname in systems that have a component.
;; Indeed, when an external program (like Guix) builds components, it needs to
;; know the name of the output.  But ASDF/SYSTEM::COMPONENT-BUILD-PATHNAME is
;; non-exported so the only reliable way to know the build pathname is to use
;; the default.
;;
;; The workaround is to set a new dummy system of which the sole purpose is to
;; produce the desired binary.

(defsystem "nyxt/gtk-application"
  :depends-on (nyxt/gtk)
  :build-operation "program-op"
  :build-pathname "nyxt"
  :entry-point "nyxt:entry-point")

(defsystem "nyxt/gobject/gtk-application"
  :depends-on (nyxt/gobject/gtk)
  :build-operation "program-op"
  :build-pathname "nyxt"
  :entry-point "nyxt:entry-point")

(defsystem "nyxt/qt-application"
  :depends-on (nyxt/qt)
  :build-operation "program-op"
  :build-pathname "nyxt-qt"
  :entry-point "nyxt:entry-point")

#+sb-core-compression
(defmethod perform ((o image-op) (c system))
  (uiop:dump-image (output-file o c)
                   :executable t
                   :compression (uiop:getenv "NYXT_COMPRESS")))

(defsystem "nyxt/install"
  :depends-on (alexandria
               str
               #.(format nil "~a-application" *nyxt-renderer*)
               nyxt/version)
  :perform (compile-op
            (o c)
            (flet ((ensure-parent-exists (file)
                     (uiop:ensure-all-directories-exist
                      (list (directory-namestring file)))))
              (let ((desktop-file (format nil "~a/applications/nyxt.desktop" *datadir*)))
                (ensure-parent-exists desktop-file)
                (with-open-file (desktop-stream desktop-file :direction :output
                                                             :if-exists :supersede)
                  (princ
                   (funcall (read-from-string "str:replace-all")
                            "VERSION"
                            (symbol-value (read-from-string "nyxt:+version+"))
                            (funcall (read-from-string "alexandria:read-file-into-string")
                                     (system-relative-pathname c "assets/nyxt.desktop")))
                   desktop-stream)))
              (mapc (lambda (icon-size)
                      (let ((icon-file (format nil "~a/icons/hicolor/~ax~a/apps/nyxt.png"
                                               *datadir* icon-size icon-size)))
                        (ensure-parent-exists icon-file)
                        (uiop:copy-file (system-relative-pathname
                                         c
                                         (format nil "assets/nyxt_~ax~a.png"
                                                 icon-size icon-size))
                                        icon-file)))
                    '(16 32 128 256 512))
              (let ((binary-file (format nil "~a/nyxt" *bindir*)))
                (ensure-parent-exists binary-file)
                (uiop:copy-file (system-relative-pathname c "nyxt") binary-file)
                ;; TODO: Use file-attributes instead of chmod?  Too verbose?
                (uiop:run-program (list "chmod" "+x" binary-file))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Library subsystems:

(defsystem "nyxt/download-manager"
  :depends-on (calispel
               cl-ppcre
               dexador
               log4cl
               quri
               str)
  :pathname "libraries/download-manager/"
  :components ((:file "package")
               (:file "engine")
               (:file "native"))
  :in-order-to ((test-op (test-op "nyxt/download-manager/tests"))))

(defsystem "nyxt/download-manager/tests"
  :depends-on (nyxt/download-manager prove)
  :perform (test-op (op c)
                    (nyxt-run-test c "libraries/download-manager/tests/"
                                   :network-needed-p t)))

(defsystem "nyxt/analysis"
  :depends-on (str
               serapeum
               alexandria
               cl-ppcre)
  :pathname "libraries/analysis/"
  :components ((:file "package")
               (:file "data")
               (:file "stem")
               (:file "tokenize")
               (:file "analysis")
               (:file "document-vector")
               (:file "text-rank")
               (:file "dbscan")))

(defsystem "nyxt/user-interface"
  :depends-on (cl-markup)
  :pathname "libraries/user-interface/"
  :components ((:file "package")
               (:file "user-interface")))

(defsystem "nyxt/text-buffer"
  :depends-on (cluffer)
  :pathname "libraries/text-buffer/"
  :components ((:file "package")
               (:file "text-buffer")))

(defsystem "nyxt/history-tree"
  :depends-on (alexandria
               cl-custom-hash-table
               local-time
               nyxt/class-star
               trivial-package-local-nicknames)
  :pathname "libraries/history-tree/"
  :components ((:file "package")
               (:file "history-tree"))
  :in-order-to ((test-op (test-op "nyxt/history-tree/tests"))))

(defsystem "nyxt/history-tree/tests"
  :depends-on (nyxt/history-tree prove)
  :perform (test-op (op c)
                    (nyxt-run-test c "libraries/history-tree/tests/")))

(defsystem "nyxt/password-manager"
  :depends-on (bordeaux-threads
               cl-ppcre
               str
               trivial-clipboard
               uiop
               nyxt/class-star)
  :pathname "libraries/password-manager/"
  :components ((:file "package")
               (:file "password")
               (:file "password-keepassxc")
               (:file "password-security")
               ;; Keep password-store last so that it has higher priority.
               (:file "password-pass")))

(defsystem "nyxt/keymap"
  :depends-on (alexandria fset str)
  :pathname "libraries/keymap/"
  :components ((:file "package")
               (:file "types")
               (:file "conditions")
               (:file "keymap")
               (:file "scheme")
               (:file "scheme-names"))
  :in-order-to ((test-op (test-op "nyxt/keymap/tests"))))

(defsystem "nyxt/keymap/tests"
  :depends-on (alexandria fset nyxt/keymap prove)
  :components ((:file "libraries/keymap/test-package"))
  :perform (test-op (op c)
                    (nyxt-run-test c "libraries/keymap/tests/")))

(defsystem "nyxt/class-star"
  :depends-on (hu.dwim.defclass-star moptilities alexandria)
  :pathname "libraries/class-star/"
  :components ((:file "package")
               (:file "patch")
               (:file "class-star"))
  :in-order-to ((test-op (test-op "nyxt/class-star/tests"))))

(defsystem "nyxt/class-star/tests"
  :depends-on (nyxt/class-star prove)
  :perform (test-op (op c)
                    (nyxt-run-test c "libraries/class-star/tests/")))

(defsystem "nyxt/ospm"
  :depends-on (alexandria
               calispel
               cl-ppcre
               local-time
               named-readtables
               #-sbcl
               osicat
               serapeum
               str
               trivia
               nyxt/class-star)
  :pathname "libraries/ospm/"
  :components ((:file "package")
               (:file "scheme-syntax")
               (:file "guix-backend")
               (:file "ospm")
               (:file "ospm-guix"))
  :in-order-to ((test-op (test-op "nyxt/ospm/tests"))))

(defsystem "nyxt/ospm/tests"
  :depends-on (nyxt/ospm prove)
  :components ((:file "libraries/ospm/test-package"))
  :perform (test-op (op c)
                    (nyxt-run-test c "libraries/ospm/tests/tests.lisp")))

(defsystem "nyxt/prompter"
  :depends-on (alexandria
               calispel
               cl-containers
               closer-mop
               mk-string-metrics
               moptilities
               serapeum
               str
               trivial-package-local-nicknames
               nyxt/class-star)
  :pathname "libraries/prompter/"
  :components ((:file "package")
               (:file "filter-preprocessor")
               (:file "filter")
               (:file "prompter-source")
               (:file "prompter"))
  :in-order-to ((test-op (test-op "nyxt/prompter/tests"))))

(defsystem "nyxt/prompter/tests"
  :depends-on (nyxt/prompter prove)
  :components ((:file "libraries/prompter/test-package"))
  :perform (test-op (op c)
                    (nyxt-run-test c "libraries/prompter/tests/")))
