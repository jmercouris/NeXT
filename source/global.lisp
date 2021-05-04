;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)
;; Packagers are welcome to customize the `defparameter's to suit the host system.

(export-always '*options*)
(defvar *options* '()
  "The list of command line options.")

(defvar *run-from-repl-p* t
  "If non-nil, don't terminate the Lisp process when quitting the browser.
This is useful when the browser is run from a REPL so that quitting does not
close the connection.")

(defvar *headless-p* nil
  "If non-nil, don't display anything.
This is convenient for testing purposes or to drive Nyxt programmatically.")

(export-always '*browser*)
(defvar *browser* nil
  "The entry-point object to a complete instance of Nyxt.
It can be initialized with

  (setf *browser* (make-instance 'user-browser))

It's possible to run multiple interfaces of Nyxt at the same time.  You can
let-bind *browser* to temporarily switch interface.")

(declaim (type hooks:hook-void *after-init-hook*))
(export-always '*after-init-hook*)
(defvar *after-init-hook* (make-instance 'hooks:hook-void)
  "The entry-point object to configure everything in Nyxt.
The hook takes no argument.

This hook is run after the `*browser*' is instantiated and before the
`startup' is run.

Add a handler can be added with:

  (hooks:add-hook *after-init-hook*
    (hooks:make-handler-void #'my-foo-function))")

(export-always '*swank-port*)
(defvar *swank-port* 4006
  "The port that Swank will open a new server on (default Emacs SLIME port
is 4005, default set to 4006 in Nyxt to avoid collisions).")

(export-always '+newline+)
(alex:define-constant +newline+ (string #\newline) :test #'equal)

(export-always '+version+)
(alex:define-constant +version+
  (or (uiop:getenv "NYXT_VERSION")      ; This is useful for build systems without Git.
      ;; TODO: Just use ASDF version when we are back to semver with 2.0.
      ;; Then remove git dependency (e.g. from build-scripts).
      (ignore-errors
       (uiop:with-current-directory ((asdf:system-source-directory :nyxt))
         (uiop:run-program (list "git" "describe" "--always" "--tags")
                           :output '(:string :stripped t))))
      (asdf/component:component-version (asdf:find-system :nyxt)))
  :test #'equal)
