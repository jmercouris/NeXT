;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun directory-elements (directory)
  "Return list of all the files and subdirectories inside DIRECTORY."
  (let ((directory (pathname directory)))
    (append (uiop:subdirectories directory)
            (uiop:directory-files directory))))

(defmethod prompter:object-attributes ((path pathname))
  ;; TODO: Add dirname, basename, extension.
  ;; It will be useful when we have per-attribute filtering.
  `(("Path" ,(namestring path))))

(defun match-extension (ext)
  (lambda (pathname)
    (string-equal (pathname-type pathname) ext)))

(defun make-file-source-preprocessor (&rest filters)
  "Return a preprocessor that lists all files satisfying all FILTERS.
It's suitable for `prompter:filter-preprocessor'."
  (lambda (suggestions source input)
    (declare (ignore suggestions))
    (let ((pathname (uiop:ensure-pathname (if (uiop:emptyp input)
                                              *default-pathname-defaults*
                                              input))))
      (prompter:filter-exact-matches
       (prompter:ensure-suggestions-list
        source
        (sera:filter
         (apply #'alex:conjoin (or filters (list #'identity)))
         (directory-elements (if (uiop:directory-pathname-p pathname)
                                 pathname
                                 (uiop:pathname-directory-pathname pathname)))))
       source
       input))))

(define-class file-source (prompter:source)
  ((prompter:name "Files")
   (prompter:filter-preprocessor (make-file-source-preprocessor))
   (prompter:multi-selection-p t)
   (open-file-in-new-buffer-p t :documentation "If nil, don't open files and directories in a new buffer.")
   (supported-media-types '("mp3" "ogg" "mp4" "flv" "wmv" "webm" "mkv")
                          :type list-of-strings
                          :documentation "Media types that Nyxt can open.
Others are opened with OS-specific mechanisms.")
   (open-file-function #'default-open-file-function
                       ;; TODO: Allow `data-path's?
                       :type (function ((or string pathname) &key
                                        (:supported-p boolean)
                                        (:new-buffer-p boolean)))
                       :documentation "The function to open the file with.
Accepts the name of the file as the first argument and has two keyword arguments:

- :supported-p denotes whether file is considered supported by Nyxt (i.e., its
  extension is one of `supported-media-types').
- :new-buffer-p says whether the file needs to be opened in a new buffer. It
  most probably will be equal to `open-file-in-new-buffer-p'."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "Prompt source for file(s) on the disk."))
(define-user-class file-source)

(define-class open-file-source (user-file-source) ())
(define-user-class open-file-source)

(defun supported-media-or-directory (filename
                                     &optional (file-source (make-instance 'user-file-source)))
  "Return T if this filename's extension is a media that Nyxt can open (or a directory).
See `supported-media-types' of `file-mode'."
  (or (and (uiop:directory-pathname-p filename)
           (uiop:directory-exists-p filename))
      (sera:and-let* ((extension (pathname-type filename))
                      (extensions (supported-media-types file-source)))
        (find extension extensions :test #'string-equal))))

(defmethod initialize-instance :after ((source open-file-source) &key)
  (setf (slot-value source 'prompter:actions)
        (append
         (list (make-command open-file* (files)
                 (let* ((new-buffer-p (open-file-in-new-buffer-p source)))
                   ;; Open first file according to `open-file-in-new-buffer-p'
                   (funcall (open-file-function source) (first files)
                            :new-buffer-p new-buffer-p
                            :supported-p (supported-media-or-directory (first files) source))
                   ;; Open the rest of the files in new buffers unconditionally.
                   (dolist (file (rest files))
                     (funcall (open-file-function source) file
                              :new-buffer-p t
                              :supported-p (supported-media-or-directory file source))))))
         (slot-value source 'prompter:actions))))

#+linux
(defvar *xdg-open-program* "xdg-open")

(export-always 'default-open-file-function)
(defun default-open-file-function (filename &key supported-p new-buffer-p)
  "Open FILENAME in Nyxt if supported, or externally otherwise.
FILENAME is the full path of the file (or directory).

See `supported-media-types' to customize the file types that are opened in
Nyxt and those that are opened externally.

NEW-BUFFER-P defines whether the file/directory is opened in a new buffer.
SUPPORTED-P says whether the file can be opened by Nyxt.

Can be used as a `open-file-function'."
  (let ((file-url (if (valid-url-p filename)
                      (quri:uri filename)
                      (quri::make-uri-file :path filename))))
    (handler-case
        (if supported-p
            (if new-buffer-p
                (make-buffer-focus :url file-url)
                (buffer-load file-url))
            (uiop:launch-program
             #+linux
             (list *xdg-open-program* (quri:render-uri file-url))
             #+darwin
             (list "open" (namestring filename))))
      ;; We can probably signal something and display a notification.
      (error (c) (log:error "Opening ~a: ~a~&" filename c)))))

(define-command open-file (&key (default-directory *default-pathname-defaults*))
  "Open a file from the filesystem.

The user is prompted with the prompt-buffer, files are browsable with
fuzzy suggestion.

DEFAULT-DIRECTORY specifies which directory to start from. Defaults to user home
directory.

By default, it uses the `xdg-open' command. The user can override the
`open-file-function' of `file-mode' which takes the filename (or
directory name) as parameter."
  (prompt
   :input (namestring default-directory)
   :prompt "Open file"
   :sources (list (make-instance 'user-open-file-source))))
