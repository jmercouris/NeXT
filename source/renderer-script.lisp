;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(export-always 'define-parenscript)
(defmacro define-parenscript (script-name args &body script-body)
  "Define parenscript function SCRIPT-NAME.
SCRIPT-BODY must be a valid parenscript and will be wrapped in (PS:PS ...).
Any Lisp expression must be wrapped in (PS:LISP ...).

The returned function sends the compiled Javascript to the current buffer webview.
The function can be passed ARGS."
  `(progn
     (defun ,script-name ,args
       (ffi-buffer-evaluate-javascript (current-buffer)
                                       (ps:ps ,@script-body)))))

(export-always 'pflet)
(defmacro pflet (functions &body body)
  (flet ((transform-definition (name args body)
           `(,name ,args
                   (ffi-buffer-evaluate-javascript (current-buffer) (ps:ps . ,body)))))
    `(flet ,(loop for (name lambda-list . body) in functions
                  collect (transform-definition name lambda-list body))
       ,@body)))

(define-parenscript %document-scroll-position (&optional (y 0 y-provided-p) (x 0 x-provided-p))
  (let ((x (ps:lisp x))
        (y (ps:lisp y)))
    (if (or (ps:lisp x-provided-p) (ps:lisp y-provided-p))
        (ps:chain window (scroll-to x y))
        (list (ps:chain window page-y-offset)
              (ps:chain window page-x-offset)))))

(export-always 'document-scroll-position)
(defmethod document-scroll-position (&optional (buffer (current-buffer)))
  "Get current scroll position or set it.
If passed no arguments, return a list of two elements: vertical (Y) and
horizontal (X) offset.
If `setf'-d to a single value (or a single list) -- set Y to it.
If `setf'-d to a list of two values -- set Y to `first' and X to `second' element."
  (with-current-buffer buffer
    (alex:when-let ((position (%document-scroll-position)))
      (mapcar #'parse-integer (str:split "," position)))))

(defmethod (setf document-scroll-position) (value &optional (buffer (current-buffer)))
  (when value
    (with-current-buffer buffer
      (destructuring-bind (y &optional x)
          (uiop:ensure-list value)
        (%document-scroll-position y x)))))

(export-always 'document-get-paragraph-contents)
(define-parenscript document-get-paragraph-contents (&key (limit 100000))
  (let ((result ""))
    (loop for element in (nyxt/ps:qsa document (list "p"))
          do (setf result (+ result
                             (ps:chain element text-content))))
    (ps:chain result (slice 0 (ps:lisp limit)))))

(export-always '%paste)
(define-parenscript %paste (&key (input-text (ring-insert-clipboard (clipboard-ring *browser*))))
  (let ((active-element (ps:chain document active-element))
        (tag (ps:chain document active-element tag-name)))
    (when (or (string= tag "INPUT")
              (string= tag "TEXTAREA"))
      (nyxt/ps:insert-at active-element (ps:lisp input-text)))))

(export-always '%copy)
(define-parenscript %copy ()
  "Return selected text from javascript."
  (ps:chain window (get-selection) (to-string)))

(defun html-write (content &optional (buffer (current-buffer)))
  (ffi-buffer-evaluate-javascript-async
   buffer
   (ps:ps (ps:chain document
                    (write (ps:lisp content))))))

(defun html-set (content &optional (buffer (current-buffer)))
  (ffi-buffer-evaluate-javascript-async
   buffer
   (ps:ps (setf (ps:@ document body |innerHTML|)
                (ps:lisp content)))))

(defun html-set-style (style-string &optional (buffer (current-buffer)))
  (let ((style (spinneret:with-html-string (:style style-string))))
    (ffi-buffer-evaluate-javascript-async
     buffer
     (ps:ps (ps:chain document body
                      (|insertAdjacentHTML| "afterbegin"
                                            (ps:lisp style)))))))

(export-always 'with-current-html-buffer)
(defmacro with-current-html-buffer ((buffer-var title mode
                                     &key no-history-p)
                                    &body body)
  "Switch to a buffer in MODE displaying BODY.
If a buffer in MODE with TITLE exists, reuse it, otherwise create a new buffer.
BUFFER-VAR is bound to the new buffer in BODY.
MODE is a mode symbol.
BODY must return the HTML markup as a string."
  (alex:once-only (title mode)
    `(let* ((,buffer-var (or (find-if (lambda (b)
                                        (and (string= (title b) ,title)
                                             (find-mode b ,mode)))
                                      (buffer-list))
                             (funcall (symbol-function ,mode)
                                      :activate t
                                      :buffer (make-internal-buffer
                                               :title ,title
                                               :no-history-p ,no-history-p)))))
       (html-set
        (progn
          ,@body)
        ,buffer-var)
       (set-current-buffer ,buffer-var)
       ,buffer-var)))

(defmacro with-current-panel ((buffer-var title &key (side :left))
                              &body body)
  "Display a panel buffer displaying CONTENT.
If a panel with TITLE exists, reuse it, otherwise create a new panel.
BUFFER-VAR is bound to the new buffer in BODY.
BODY must return the HTML markup as a string."
  (alex:once-only (title)
    `(let* ((window (current-window))
            (,buffer-var (or (find ,title
                                   (panel-buffers window)
                                   :key #'title
                                   :test #'string=)
                             (make-instance 'user-panel-buffer
                                            :title ,title))))
       (window-add-panel-buffer window ,buffer-var ,side)
       (html-set
        (progn
          ,@body)
        ,buffer-var)
       ,buffer-var)))
