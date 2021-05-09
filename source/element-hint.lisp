;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/web-mode)

(define-parenscript add-stylesheet ()
  (unless (ps:chain document (query-selector "#nyxt-stylesheet"))
    (ps:try
     (ps:let* ((style-element (ps:chain document (create-element "style")))
               (box-style (ps:lisp (box-style (current-mode 'web))))
               (highlighted-style (ps:lisp (highlighted-box-style (current-mode 'web)))))
       (setf (ps:@ style-element id) "nyxt-stylesheet")
       (ps:chain document head (append-child style-element))
       (ps:chain style-element sheet (insert-rule box-style 0))
       (ps:chain style-element sheet (insert-rule highlighted-style 1)))
     (:catch (error)))))

(define-parenscript hint-elements (nyxt-identifiers hints)
  (defun hint-determine-position (rect)
    "Determines the position of a hint according to the element"
    (ps:create :top  (+ (ps:@ window page-y-offset) (ps:@ rect top))
               :left (+ (ps:@ window page-x-offset) (ps:@ rect left))))

  (defun hint-create-element (original-element hint)
    "Creates a DOM element to be used as a hint"
    (ps:let* ((rect (ps:chain original-element (get-bounding-client-rect)))
              (position (hint-determine-position rect))
              (element (ps:chain document (create-element "span"))))
      (setf (ps:@ element class-name) "nyxt-hint")
      (setf (ps:@ element style position) "absolute")
      (setf (ps:@ element style left) (+ (ps:@ position left) "px"))
      (setf (ps:@ element style top) (+ (ps:@ position top) "px"))
      (setf (ps:@ element id) (+ "nyxt-hint-" hint))
      (setf (ps:@ element text-content) hint)
      element))

  (dotimes (i (ps:lisp (length nyxt-identifiers)))
    (let ((element (nyxt/ps:qs document (+ "[nyxt-identifier=\""
                                           (aref (ps:lisp (list 'quote nyxt-identifiers)) i) "\"]")))
          (hint (aref (ps:lisp (list 'quote hints)) i)))
      (when element
        (ps:chain element (set-attribute "nyxt-hint" hint))
        (ps:let ((hint-element (hint-create-element element hint)))
          (ps:chain document body (append-child hint-element)))))))

(defun add-element-hints (&key (selectors '("a" "button" "input" "textarea")))
  ;; TODO: Use hint generator from https://github.com/atlas-engineer/nyxt/issues/1290 maybe?
  (labels ((select-from-alphabet (code char-length alphabet)
             (let* ((exponents (nreverse (loop for pow below char-length
                                               collect (expt (length alphabet) pow)))))
               (coerce (loop for exp in exponents
                             for quotinent = (floor (/ code exp))
                             collect (aref alphabet quotinent)
                             do (decf code (* quotinent exp)))
                       'string)))
           (generate-hints (length)
             (let* ((alphabet (hints-alphabet (current-mode 'web)))
                    (char-length (ceiling (log length (length alphabet)))))
               (loop for i below length collect (select-from-alphabet i char-length alphabet)))))
    (let* ((dom (update-document-model))
           (hintable-elements (alex:mappend #'(lambda (selector)
                                                (coerce (clss:select selector dom) 'list))
                                            selectors))
           (hints (generate-hints (length hintable-elements))))
      (add-stylesheet)
      (hint-elements (mapcar #'get-nyxt-id hintable-elements) hints)
      (mapcar #'(lambda (elem hint)
                  (plump:set-attribute elem "nyxt-hint" hint)
                  elem)
              hintable-elements hints))))

(define-parenscript remove-element-hints ()
  (defun hints-remove-all ()
    "Removes all the elements"
    (ps:dolist (element (nyxt/ps:qsa document ".nyxt-hint"))
      (ps:chain element (remove))))
  (hints-remove-all))

(define-parenscript click-element (&key nyxt-identifier)
  (ps:chain (nyxt/ps:qs document (ps:lisp (format nil "[nyxt-identifier=\"~a\"]" nyxt-identifier))) (click)))

(define-parenscript focus-element (&key nyxt-identifier)
  (ps:chain (nyxt/ps:qs document (ps:lisp (format nil "[nyxt-identifier=\"~a\"]" nyxt-identifier))) (focus))
  (ps:chain (nyxt/ps:qs document (ps:lisp (format nil "[nyxt-identifier=\"~a\"]" nyxt-identifier))) (select)))

(define-parenscript highlight-selected-hint (&key element scroll)
  (defun update-hints ()
    (ps:let* ((new-element (nyxt/ps:qs document (ps:lisp (format nil "#nyxt-hint-~a"
                                                         (plump:get-attribute element "nyxt-hint"))))))
      (when new-element
        (unless ((ps:@ new-element class-list contains) "nyxt-highlight-hint")
          (ps:let ((old-elements (nyxt/ps:qsa document ".nyxt-highlight-hint")))
            (ps:dolist (e old-elements)
              (setf (ps:@ e class-name) "nyxt-hint"))))
        (setf (ps:@ new-element class-name) "nyxt-hint nyxt-highlight-hint")
        (if (ps:lisp scroll)
            (ps:chain new-element (scroll-into-view
                                   (ps:create block "nearest")))))))

  (update-hints))

(define-parenscript remove-focus ()
  (ps:let ((old-elements (nyxt/ps:qsa document ".nyxt-highlight-hint")))
    (ps:dolist (e old-elements)
      (setf (ps:@ e class-name) "nyxt-hint"))))

(define-class hint-source (prompter:source)
  ((prompter:name "Hints")
   (prompter:follow-p t)
   (prompter:filter-postprocessor
    (lambda (suggestions source input)
      (declare (ignore source))
      (multiple-value-bind (matching-hints other-hints)
          (sera:partition
           (lambda (element)
             (str:starts-with-p input (plump:get-attribute element "nyxt-hint") :ignore-case t))
           suggestions
           :key #'prompter:value)
        (append matching-hints other-hints))))
   (prompter:follow-mode-functions (lambda (suggestion)
                                     (highlight-selected-hint :element suggestion)))))

(serapeum:export-always 'query-hints)
(defun query-hints (prompt function &key multi-selection-p)
  (let* ((buffer (current-buffer)))
    (let ((result (prompt
                   :prompt prompt
                   :extra-modes '(element-hint-mode)
                   :history nil
                   :sources
                   (make-instance
                    'hint-source
                    :multi-selection-p multi-selection-p
                    :constructor (add-element-hints))
                   :after-destructor
                   (lambda ()
                     (with-current-buffer buffer
                       (remove-element-hints))))))
      (funcall function result))))

(defmethod prompter:object-attributes ((input nyxt/dom:input-element))
  `(("Hint" ,(plump:get-attribute input "nyxt-hint"))
    ,@(when (plump:get-attribute input "placeholder")
        (list `("Placeholder" ,(plump:get-attribute input "placeholder"))))
    ("Type" "Input")))

(defmethod prompter:object-attributes ((textarea nyxt/dom:textarea-element))
  `(("Hint" ,(plump:get-attribute textarea "nyxt-hint"))
    ,@(when (plump:get-attribute textarea "placeholder")
        (list `("Placeholder" ,(plump:get-attribute textarea "placeholder"))))
    ,@(when (plump:get-attribute textarea "value")
        (list `("Value" ,(str:shorten 80 (plump:get-attribute textarea "value")))))
    ("Type" "Textarea")))

(defmethod prompter:object-attributes ((a nyxt/dom:a-element))
  `(("Hint" ,(plump:get-attribute a "nyxt-hint"))
    ,@(when (plump:get-attribute a "href")
        (list `("URL" ,(plump:get-attribute a "href"))))
    ,@(when (plump:children a)
        (list `("Body" ,(plump:text a))))
    ("Type" "Link")))

(defmethod prompter:object-attributes ((button nyxt/dom:button-element))
  `(("Hint" ,(plump:get-attribute button "nyxt-hint"))
    ,@(when (plump:children button)
        (list `("Body" ,(plump:text button))))
    ("Type" "Button")))

(defmethod %follow-hint ((a nyxt/dom:a-element))
  (click-element :nyxt-identifier (get-nyxt-id a)))

(defmethod %follow-hint ((button nyxt/dom:button-element))
  (click-element :nyxt-identifier (get-nyxt-id button)))

(defmethod %follow-hint ((input nyxt/dom:input-element))
  (focus-element :nyxt-identifier (get-nyxt-id input)))

(defmethod %follow-hint ((textarea nyxt/dom:textarea-element))
  (focus-element :nyxt-identifier (get-nyxt-id textarea)))

(defmethod %follow-hint-new-buffer-focus ((a nyxt/dom:a-element) &optional parent-buffer)
  (make-buffer-focus :url (plump:get-attribute "href" a)
                     :parent-buffer parent-buffer
                     :nosave-buffer-p (nosave-buffer-p parent-buffer)))

(defmethod %follow-hint-new-buffer-focus ((element plump:element) &optional parent-buffer)
  (declare (ignore parent-buffer))
  (echo "Unsupported operation for hint: can't open in new buffer."))

(defmethod %follow-hint-new-buffer ((a nyxt/dom:a-element) &optional parent-buffer)
  (make-buffer :url (plump:get-attribute "href" a) :parent-buffer parent-buffer))

(defmethod %follow-hint-new-buffer ((element plump:element) &optional parent-buffer)
  (declare (ignore parent-buffer))
  (echo "Unsupported operation for hint: can't open in new buffer."))

(defmethod %follow-hint-nosave-buffer-focus ((a nyxt/dom:a-element))
  (make-buffer-focus :url (plump:get-attribute "href" a) :nosave-buffer-p t))

(defmethod %follow-hint-nosave-buffer-focus ((element plump:element))
  (echo "Unsupported operation for hint: can't open in new buffer."))

(defmethod %follow-hint-nosave-buffer ((a nyxt/dom:a-element))
  (make-nosave-buffer :url (plump:get-attribute "href" a)))

(defmethod %follow-hint-nosave-buffer ((element plump:element))
  (echo "Unsupported operation for hint: can't open in new buffer."))

(defmethod %copy-hint-url ((a nyxt/dom:a-element))
  (trivial-clipboard:text (plump:get-attribute "href" a)))

(defmethod %copy-hint-url ((element plump:element))
  (echo "Unsupported operation for hint: can't copy URL."))

(defun prompt-buffer-selection-highlight-hint (&key suggestions scroll follow
                                                 (prompt-buffer (current-prompt-buffer))
                                                 (buffer (current-buffer)))
  (let ((hint (flet ((hintp (hint-suggestion)
                       (if (typep hint-suggestion '(or plump:element search-match))
                           hint-suggestion
                           nil)))
                (if suggestions
                    (hintp (prompter:value (first suggestions)))
                    (when prompt-buffer
                      (hintp (current-suggestion-value)))))))
    (when hint
      (when (and follow
                 (slot-exists-p hint 'buffer)
                 (not (equal (buffer hint) buffer)))
        (set-current-buffer (buffer hint))
        (setf buffer (buffer hint)))
      (if (or
           (not (slot-exists-p hint 'buffer))
           (and (slot-exists-p hint 'buffer)
                (equal (buffer hint) buffer)))
          (with-current-buffer buffer
            (highlight-selected-hint :element hint
                                     :scroll scroll))
          (remove-focus)))))

(define-command follow-hint ()
  "Show a set of element hints, and go to the user inputted one in the current
buffer."
  (query-hints "Go to element" (lambda (results) (%follow-hint (first results)))))

(define-command follow-hint-new-buffer ()
  "Show a set of element hints, and open the user inputted one in a new
buffer (not set to visible active buffer)."
  (let ((buffer (current-buffer)))
    (query-hints "Open element in new buffer"
                 (lambda (result) (mapcar (alex:rcurry #'%follow-hint-new-buffer buffer)
                                          result))
                 :multi-selection-p t)))

(define-command follow-hint-new-buffer-focus ()
  "Show a set of element hints, and open the user inputted one in a new
visible active buffer."
  (let ((buffer (current-buffer)))
    (query-hints "Go to element in new buffer"
                 (lambda (result)
                   (%follow-hint-new-buffer-focus (first result) buffer)
                   (mapcar (alex:rcurry #'%follow-hint-new-buffer buffer)
                           (rest result)))
                 :multi-selection-p t)))

(define-command follow-hint-nosave-buffer ()
  "Show a set of element hints, and open the user inputted one in a new
nosave buffer (not set to visible active buffer)."
  (query-hints "Open element in new buffer"
               (lambda (result) (mapcar #'%follow-hint-nosave-buffer result))
               :multi-selection-p t))

(define-command follow-hint-nosave-buffer-focus ()
  "Show a set of element hints, and open the user inputted one in a new
visible nosave active buffer."
  (query-hints "Go to element in new buffer"
               (lambda (result)
                 (%follow-hint-nosave-buffer-focus (first result))
                 (mapcar #'%follow-hint-nosave-buffer (rest result)))
               :multi-selection-p t))

(define-command copy-hint-url ()
  "Show a set of element hints, and copy the URL of the user inputted one."
  (query-hints "Copy element URL" (lambda (result)  (%copy-hint-url (first result)))))

(define-command bookmark-hint ()
  "Show link hints on screen, and allow the user to bookmark one"
  (query-hints "Bookmark hint"
               (lambda (result)
                 (let ((url (url (first result))))
                   (bookmark-url :url url)))
               :multi-selection-p t))

(define-command download-hint-url ()
  "Download the file under the URL(s) hinted by the user."
  (let ((buffer (current-buffer)))
    (query-hints "Download link URL"
                 (lambda (selected-links)
                   (loop for link in selected-links
                         ;; TODO: sleep should NOT be necessary to avoid breaking download
                         do (download buffer (url link))
                            (sleep 0.25)))
                 :multi-selection-p t)))
