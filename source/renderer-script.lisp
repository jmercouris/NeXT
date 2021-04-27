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

(export-always 'document-get-paragraph-contents)
(define-parenscript document-get-paragraph-contents (&key (limit 100000))
  (defun qsa (context selector)
    (ps:chain context (query-selector-all selector)))
  (let ((result ""))
    (loop for element in (qsa document (list "p"))
          do (setf result (+ result
                             (ps:chain element text-content))))
    (ps:chain result (slice 0 (ps:lisp limit)))))

(export-always '%paste)
(define-parenscript %paste (&key (input-text (ring-insert-clipboard (clipboard-ring *browser*))))
  (defun insert-at (tag input-text)     ; TODO: Factor with `prompt-buffer-paste'.
    (let ((start (ps:chain tag selection-start))
          (end (ps:chain tag selection-end)))
      (setf (ps:chain tag value)
            (+ (ps:chain tag value (substring 0 start))
               input-text
               (ps:chain tag value
                         (substring end
                                    (ps:chain tag value length)))))
      (if (= start end)
          (progn
            (setf (ps:chain tag selection-start) (+ start (ps:chain input-text length)))
            (setf (ps:chain tag selection-end) (ps:chain tag selection-start)))
          (progn
            (setf (ps:chain tag selection-start) start)
            (setf (ps:chain tag selection-end) (+ start (ps:chain input-text length)))))))
  (let ((active-element (ps:chain document active-element))
        (tag (ps:chain document active-element tag-name)))
    (when (or (string= tag "INPUT")
              (string= tag "TEXTAREA"))
      (insert-at active-element (ps:lisp input-text)))))

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
  (let ((style (markup:markup (:style style-string))))
    (ffi-buffer-evaluate-javascript-async
     buffer
     (ps:ps (ps:chain document body
                      (|insertAdjacentHTML| "afterbegin"
                                            (ps:lisp style)))))))

(defun fetch-links (&optional (buffer (current-buffer))
                      (filtering-rules (list #'host=
                                             #'distinct-url-path-p
                                             #'scheme=)))
  "Return a list of links from BUFFER.
FILTERING-RULES is a list of functions that take two URLs and return a boolean.
A link is collected when, for all elements of FILTERING-RULES, the return value
is non-nil."
  (with-current-buffer buffer
    (pflet ((nlinks () (ps:chain document links length))
            (fetch-link (i) (ps:chain document links (item (ps:lisp i)) href)))
      (loop for i from 0 to (1- (parse-integer (nlinks)))
            with url = (url buffer)              ; computed once
            for link = (quri:uri (fetch-link i)) ; computed on every iteration
            when (eq-uri-p url link filtering-rules)
              collect link))))

(export-always 'with-current-html-buffer)
(defmacro with-current-html-buffer ((buffer-var title mode
                                     &key no-history-p)
                                    &body body)
  "Switch to a buffer in MODE displaying CONTENT.
If a buffer in MODE with TITLE exists, reuse it, otherwise create a new buffer.
BUFFER-VAR is bound to the new buffer in BODY.
MODE is a mode symbol.
BODY must return the HTML markup as a string."
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
     ,buffer-var))
