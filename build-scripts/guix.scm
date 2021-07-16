;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

;;; Commentary:
;;
;; GNU Guix development package.  To build and install, clone this repository,
;; switch directory to here and run:
;;
;;   guix package --install-from-file=build-scripts/guix.scm
;;
;; To use as the basis for a development environment, run:
;;
;;   guix environment --container --load=build-scripts/guix.scm --ad-hoc glib glib-networking gsettings-desktop-schemas
;;
;; Replace --container by --pure if you still want ASDF to see external
;; libraries in ~/common-lisp, etc.
;; To build a local executable and then run it:
;;
;;   guix environment --container --load=build-scripts/guix.scm -- make all NYXT_INTERNAL_QUICKLISP=false
;;   guix environment --pure --load=build-scripts/guix.scm -- ./nyxt
;;
;; To start in a container, run:
;;
;;   guix environment --load=build-scripts/guix.scm --container --network --share=/PATH/TO/YOUR/NYXT/CHECKOUT=/nyxt --preserve='^DISPLAY$' --expose=/etc/ssl/certs --ad-hoc nss-certs glib glib-networking gsettings-desktop-schemas
;;
;; Replace '/PATH/TO/YOUR/NYXT/CHECKOUT' as appropriate.
;; Then in the container environment:
;;
;;   cd /nyxt
;;   make all NYXT_INTERNAL_QUICKLISP=false
;;   ./nyxt
;;
;;; Code:

(use-modules (ice-9 match)
             (ice-9 popen)
             (ice-9 rdelim)
             (srfi srfi-1)
             (srfi srfi-26)
             ((guix build utils) #:select (with-directory-excursion))
             (guix gexp)
             (guix packages)
             ((guix licenses) #:prefix license:)
             (guix licenses)
             (guix git-download)
             (guix build-system gnu)
             (guix build-system glib-or-gtk)
             (gnu packages)
             (gnu packages base)
             (gnu packages commencement)
             (gnu packages glib)
             (gnu packages lisp)
             (gnu packages lisp-xyz)
             (gnu packages gnome)
             (gnu packages gstreamer)
             (gnu packages gtk)
             (gnu packages pkg-config)
             (gnu packages version-control)
             (gnu packages webkit))

(define %source-dir (dirname (dirname (current-filename))))

(define git-file?
  (let* ((pipe (with-directory-excursion %source-dir
                 (open-pipe* OPEN_READ "git" "ls-files")))
         (files (let loop ((lines '()))
                  (match (read-line pipe)
                    ((? eof-object?)
                     (reverse lines))
                    (line
                     (loop (cons line lines))))))
         (status (close-pipe pipe)))
    (lambda (file stat)
      (match (stat:type stat)
        ('directory
         #t)
        ((or 'regular 'symlink)
         (any (cut string-suffix? <> file) files))
        (_
         #f)))))

(define (nyxt-git-version)              ; Like Nyxt's `+version+'.
  (let* ((pipe (with-directory-excursion %source-dir
                 (open-pipe* OPEN_READ "git" "describe" "--always" "--tags")))
         (version (read-line pipe)))
    (close-pipe pipe)
    version))

(define-public nyxt
  (package
    (name "nyxt")
    (version (nyxt-git-version))
    (source (local-file %source-dir #:recursive? #t #:select? git-file?))
    (build-system gnu-build-system)     ; TODO: Use glib-or-gtk-build-system instead?
    (arguments
     `(#:make-flags (list "nyxt" "NYXT_INTERNAL_QUICKLISP=false"
                          (string-append "DESTDIR=" (assoc-ref %outputs "out"))
                          "PREFIX=")
       #:strip-binaries? #f             ; Stripping breaks SBCL binaries.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'fix-common-lisp-cache-folder
           (lambda _
             (setenv "HOME" "/tmp")
             #t))
         (add-before 'check 'configure-tests
           (lambda _
             (setenv "NYXT_TESTS_NO_NETWORK" "1")
             (setenv "NYXT_TESTS_ERROR_ON_FAIL" "1")
             #t))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((bin (string-append (assoc-ref outputs "out") "/bin/nyxt"))
                    (glib-networking (assoc-ref inputs "glib-networking"))
                    (libs '("gsettings-desktop-schemas"))
                    (path (string-join
                           (map (lambda (lib)
                                  (string-append (assoc-ref inputs lib) "/lib"))
                                libs)
                           ":"))
                    (gi-path (getenv "GI_TYPELIB_PATH"))
                    (xdg-path (string-join
                               (map (lambda (lib)
                                      (string-append (assoc-ref inputs lib) "/share"))
                                    libs)
                               ":")))
               (wrap-program bin
                 `("GIO_EXTRA_MODULES" prefix
                   (,(string-append glib-networking "/lib/gio/modules")))
                 `("GI_TYPELIB_PATH" prefix (,gi-path))
                 `("LD_LIBRARY_PATH" ":" prefix (,path))
                 `("XDG_DATA_DIRS" ":" prefix (,xdg-path)))
               #t))))))
    ;; We use `cl-*' inputs and not `sbcl-*' ones so that CCL users can also use
    ;; this Guix manifests.
    ;;
    ;; Another reason is to not fail when an input dependency is found in
    ;; ~/common-lisp, which would trigger a rebuild of the SBCL input in the
    ;; store, which is read-only and would thus fail.
    ;;
    ;; The official Guix package should use `sbcl-*' inputs though.
    (native-inputs
     `(("prove" ,cl-prove)
       ("sbcl" ,sbcl)
       ;; Only for development, unneeded for the upstream Guix package:
       ("cl-trivial-benchmark" ,cl-trivial-benchmark)
       ;; To generate the right version in Nyxt.  Unneeded for the upstream Guix package.
       ("git" ,git-minimal)))
    (inputs
     `(("alexandria" ,cl-alexandria)
       ("bordeaux-threads" ,cl-bordeaux-threads)
       ("cl-calispel" ,cl-calispel)
       ("cl-containers" ,cl-containers)
       ("cl-css" ,cl-css)
       ("cl-custom-hash-table" ,cl-custom-hash-table)
       ("cl-html-diff" ,cl-html-diff)
       ("cl-json" ,cl-json)
       ("cl-markup" ,cl-markup)
       ("cl-ppcre" ,cl-ppcre)
       ("cl-prevalence" ,cl-prevalence)
       ("closer-mop" ,cl-closer-mop)
       ("cluffer" ,cl-cluffer)
       ("dexador" ,cl-dexador)
       ("enchant" ,cl-enchant)
       ("fset" ,cl-fset)
       ("hu.dwim.defclass-star" ,cl-hu.dwim.defclass-star)
       ("iolib" ,cl-iolib)
       ("local-time" ,cl-local-time)
       ("log4cl" ,cl-log4cl)
       ("mk-string-metrics" ,cl-mk-string-metrics)
       ("moptilities" ,cl-moptilities)
       ("named-readtables" ,cl-named-readtables)
       ;; ("osicat" ,cl-osicat) ; Not needed for SBCL.
       ("parenscript" ,cl-parenscript)
       ("plump" ,cl-plump)
       ("clss" ,cl-clss)
       ("quri" ,cl-quri)
       ("serapeum" ,cl-serapeum)
       ("spinneret" ,cl-spinneret)
       ("str" ,cl-str)
       ("swank" ,cl-slime-swank)
       ("trivia" ,cl-trivia)
       ("trivial-clipboard" ,cl-trivial-clipboard)
       ("trivial-features" ,cl-trivial-features)
       ("trivial-garbage" ,cl-trivial-garbage)
       ("trivial-package-local-nicknames" ,cl-trivial-package-local-nicknames)
       ("trivial-types" ,cl-trivial-types)
       ("unix-opts" ,cl-unix-opts)
       ;; System deps
       ("gcc" ,gcc-toolchain)           ; Needed for cl-iolib.
       ;; WebKitGTK deps
       ("cl-cffi-gtk" ,cl-cffi-gtk)
       ("cl-webkit" ,cl-webkit)
       ("glib-networking" ,glib-networking)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ;; GObjectIntrospection
       ("cl-gobject-introspection" ,cl-gobject-introspection)
       ("gtk" ,gtk+)                    ; For the main loop.
       ("webkitgtk" ,webkitgtk)         ; Required when we use its typelib.
       ("gobject-introspection" ,gobject-introspection)))
    (synopsis "Extensible web-browser in Common Lisp")
    (home-page "https://nyxt.atlas.engineer")
    (description "Nyxt is a keyboard-oriented, extensible web-browser
designed for power users.  The application has familiar Emacs and VI
key-bindings and is fully configurable and extensible in Common Lisp.")
    (license license:bsd-3)))

nyxt
