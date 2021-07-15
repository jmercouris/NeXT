;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/web-extensions
  (:use :common-lisp :nyxt)
  (:import-from #:class-star #:define-class)
  (:import-from #:serapeum #:export-always)
  (:documentation "WebExtensions API conformance code."))
(in-package :nyxt/web-extensions)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria)
  (trivial-package-local-nicknames:add-package-local-nickname :sera :serapeum)
  (trivial-package-local-nicknames:add-package-local-nickname :hooks :serapeum/contrib/hooks))

(defun load-js-file (file buffer mode)
  "Load JavaScript code from a file into the BUFFER."
  (ffi-buffer-evaluate-javascript
   buffer (uiop:read-file-string (uiop:merge-pathnames* file (extension-directory mode)))))

(defun load-css-file (file buffer mode)
  "Load CSS from the FILE and inject it into the BUFFER document."
  (nyxt::html-set-style (uiop:read-file-string (uiop:merge-pathnames* file (extension-directory mode))) buffer))

(defun make-activate-content-scripts-handler (mode name)
  (nyxt::make-handler-buffer
   (lambda (buffer)
     (dolist (script (content-scripts mode))
       (when (funcall (matching-filter script) (render-url (url buffer)))
         (dolist (js-file (js-files script))
           (load-js-file js-file buffer mode))
         (dolist (css-file (css-files script))
           (load-css-file css-file buffer mode)))
       url))
   :name name))

(define-class content-script ()
  ((matching-filter (error "Matching filter is required")
                    :type function
                    :documentation "When to activate the content script.
A function that takes a URL designator and returns t if it needs to be activated
for a given URL, and nil otherwise")
   (js-files nil
             :type list
             :documentation "JavaScript files to load.")
   (css-files nil
              :type list
              :documentation "Stylesheets to load."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(defun make-content-script (&key (matches (error "Matches key is mandatory")) js css)
  ;; TODO: Replace "/*" with ".*"? Requires regexps and some smartness, though.
  (let ((sanitize-mozilla-regex (alex:curry #'str:replace-using '("*." "*"
                                                                  "*" ".*"
                                                                  "?" ".?"))))
    (make-instance
     'content-script
     :matching-filter (apply #'match-regex
                             (mapcar sanitize-mozilla-regex (uiop:ensure-list matches)))
     :js-files (uiop:ensure-list js)
     :css-files (uiop:ensure-list css))))

(define-mode extension ()
  "The base mode for any extension to inherit from."
  ((name (error "Extension should have a name")
         :type string)
   (version (error "Extension should have a version")
            :type string)
   (description nil
                :type (or null string))
   (homepage-url nil
                 :type (or null quri:uri))
   (extension-directory nil
              :type (or null pathname)
              :documentation "The directory that the extension resides in.")
   (content-scripts nil
                    :type list
                    :documentation "A list of `content-script's used by this extension.")
   (handler-names nil
                  :type list)
   (destructor (lambda (mode)
                 (loop for name in (handler-names mode)
                       for hook in (list (buffer-loaded-hook (buffer mode)))
                       do (hooks:remove-hook hook name))))
   (constructor (lambda (mode)
                  (let ((content-script-name (gensym)))
                    (hooks:add-hook (buffer-loaded-hook (buffer mode))
                                    (make-activate-content-scripts-handler mode content-script-name))
                    (push content-script-name (handler-names mode)))))))

(defun read-png-as-base64 (png-file)
  (let ((arr (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer t)))
    (with-open-file (png png-file :element-type '(unsigned-byte 8))
      (loop for byte = (read-byte png nil nil)
            while byte
            do (vector-push-extend byte arr)
            finally (return (base64:usb8-array-to-base64-string arr))))))

(defun make-extension-icon (icons-json extension-directory extension-name)
  (format nil "<img src=\"data:image/png;base64,~a\" alt=\"~a\"
height=~a/>"
          (read-png-as-base64
           (uiop:merge-pathnames* (rest (first icons-json)) extension-directory))
          extension-name
          (- (nyxt:height (status-buffer (current-window))) 10)))

(export-always 'load-web-extension)
(defmacro load-web-extension (lispy-name directory)
  "Make an extension from DIRECTORY accessible as Nyxt mode (under LISPY-NAME).
DIRECTORY should be the one containing manifest.json file for the extension in question."
  (let* ((directory (uiop:parse-native-namestring directory))
         (json (with-open-file
                   (manifest.json (uiop:merge-pathnames* "manifest.json" directory))
                 (json:decode-json-from-source manifest.json))))
    `(progn
       (define-mode ,lispy-name (extension)
         ,(alex:assoc-value json :description)
         ((name ,(alex:assoc-value json :name))
          (version ,(alex:assoc-value json :version))
          (description ,(alex:assoc-value json :description))
          (extension-directory ,directory)
          (nyxt:glyph ,(make-extension-icon (alex:assoc-value json :icons) directory (alex:assoc-value json :name)))
          (homepage-url ,(alex:assoc-value json :homepage--url))
          (browser-action )
          (content-scripts (list ,@(mapcar (lambda (content-script-alist)
                                             (apply #'make-content-script
                                                    (alex:alist-plist content-script-alist)))
                                           (alex:assoc-value json :content--scripts)))))))))
