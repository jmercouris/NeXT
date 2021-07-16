;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/editor-mode
    (:use :common-lisp :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "Mode for editors."))
(in-package :nyxt/editor-mode)

(define-mode editor-mode ()
  "Mode for editor modes to extend. Importantly, it is required to implement the
methods get/set-content for each editor-mode. This will allow your mode to
get/set-content (which is necessary for operation)."
  ((keymap-scheme (define-scheme "editor"
                    scheme:cua
                    (list
                     "C-o" 'editor-open-file
                     "C-s" 'editor-write-file)
                    scheme:emacs
                    (list
                     "C-x C-f" 'editor-open-file
                     "C-x C-s" 'editor-write-file)
                    scheme:vi-normal
                    (list
                     "C-o" 'editor-open-file))
                  :type keymap:scheme)
   (constructor
    (lambda (mode)
      (initialize-display mode))))
  (:documentation "This class is used to define a protocol for editors to implement."))

(defmethod initialize-display ((editor editor-mode))
  (let* ((content (spinneret:with-html-string
                   (:head (:style (style (buffer editor))))
                   (:body (:p "Please configure an editor mode to use an editor buffer."))))
         (insert-content (ps:ps (ps:chain document (write (ps:lisp content))))))
    (ffi-buffer-evaluate-javascript-async (buffer editor) insert-content)))

(defmethod editor ((editor-buffer editor-buffer))
  (find-submode editor-buffer 'editor-mode))

;; IMPORTANT: Implement this method specializing on your class extending editor-mode.
(defgeneric get-content (editor)
  (:documentation "Get the content of the editor."))

;; IMPORTANT: Implement this method specializing on your class extending editor-mode.
(defgeneric set-content (editor content)
  (:documentation "Set the content of the editor."))

(defmethod write-file-with-editor ((buffer editor-buffer) &key (if-exists :error))
  (alexandria:if-let ((editor (editor buffer)))
    (alexandria:write-string-into-file (get-content editor)
                                       (file buffer)
                                       :if-exists if-exists)
    (echo "Editor buffer cannot write file without configured editor mode.")))

(defmethod open-file-with-editor ((buffer editor-buffer) file)
  (alexandria:if-let ((editor (editor buffer)))
    (if (uiop:file-exists-p file)
        (set-content editor (uiop:read-file-string file))
        (set-content editor ""))
    (echo "Editor buffer cannot open file without configured editor mode.")))

(define-command editor-open-file (&key (buffer (current-buffer)))
  "Open a file in the internal editor."
  (let ((file (first (prompt
                      :prompt "Open file"
                      :input (namestring (uiop:getcwd))
                      :sources
                      (list (make-instance 'user-file-source
                                           :name "Absolute file path"
                                           :actions '(identity))
                            (make-instance 'prompter:raw-source
                                           :name "New file"))))))
    (open-file-with-editor buffer file)
    ;; TODO: Maybe make `editor-mode' and `editor-buffer' pathname-friendly?
    (setf (file buffer) (namestring file))
    (setf (title buffer) (namestring file))
    (setf (url buffer) (quri::make-uri-file :path (namestring file)))))

(define-command editor-write-file (&key (buffer (current-buffer)) (if-exists :error))
  "Write the FILE of the BUFFER to storage."
  (write-file-with-editor buffer :if-exists if-exists)
  (echo "File ~a written to storage." (file buffer)))

(define-command-global open-new-editor-with-file ()
  "Open a new editor and query a file."
  (let ((buffer (make-editor-buffer)))
    (set-current-buffer buffer)
    (editor-open-file :buffer buffer)))
