;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/web-mode)

(define-parenscript add-element-hints (&key annotate-visible-only-p)
  (defun code-char (n)
    "Alias of String.fromCharCode"
    (ps:chain -string (from-char-code n)))

  (defun add-stylesheet ()
    (unless (nyxt/ps:qs document "#nyxt-stylesheet")
      (ps:try
       (ps:let* ((style-element (ps:chain document (create-element "style")))
                 (box-style (ps:lisp (box-style (current-mode 'web))))
                 (highlighted-style (ps:lisp (highlighted-box-style (current-mode 'web)))))
         (setf (ps:@ style-element id) "nyxt-stylesheet")
         (ps:chain document head (append-child style-element))
         (ps:chain style-element sheet (insert-rule box-style 0))
         (ps:chain style-element sheet (insert-rule highlighted-style 1)))
       (:catch (error)))))

  (defun hint-determine-position (rect)
    "Determines the position of a hint according to the element"
    (ps:create :top  (+ (ps:@ window page-y-offset) (ps:@ rect top))
               :left (+ (ps:@ window page-x-offset) (ps:@ rect left))))

  (defun hint-create-element (element hint)
    "Creates a DOM element to be used as a hint"
    (ps:let* ((rect (ps:chain element (get-bounding-client-rect)))
              (position (hint-determine-position rect))
              (element (ps:chain document (create-element "span"))))
      (setf (ps:@ element class-name) "nyxt-hint")
      (setf (ps:@ element style position) "absolute")
      (setf (ps:@ element style left) (+ (ps:@ position left) "px"))
      (setf (ps:@ element style top) (+ (ps:@ position top) "px"))
      (setf (ps:@ element id) (+ "nyxt-hint-" hint))
      (setf (ps:@ element text-content) hint)
      element))

  (defun hint-add (element hint)
    "Adds a hint on a single element. Additionally sets a unique
identifier for every hinted element."
    (ps:chain element (set-attribute "nyxt-identifier" hint))
    (ps:let ((hint-element (hint-create-element element hint)))
      (ps:chain document body (append-child hint-element))))

  (defun object-create (element hint)
    (cond ((equal "A" (ps:@ element tag-name))
           (ps:create "type" "link" "hint" hint "href" (ps:@ element href) "body" (ps:@ element |innerHTML|)))
          ((equal "BUTTON" (ps:@ element tag-name))
           (ps:create "type" "button" "hint" hint "identifier" hint "body" (ps:@ element |innerHTML|)))
          ((equal "INPUT" (ps:@ element tag-name))
           (ps:create "type" "input" "hint" hint "identifier" hint "placeholder" (ps:@ element placeholder)))
          ((equal "TEXTAREA" (ps:@ element tag-name))
           (ps:create "type" "textarea" "hint" hint "identifier" hint "placeholder" (ps:@ element placeholder)))
          ((equal "IMG" (ps:@ element tag-name))
           (ps:create "type" "img" "hint" hint "identifier" hint "src" (ps:@ element src) "alt" (ps:@ element alt)))
          ((ps:@ element onclick)
           (ps:create "type" "clickable" "hint" hint "identifier" hint "body" (ps:@ element |innerHTML|)))
          ((ps:@ element onfocus)
           (ps:create "type" "focusable" "hint" hint "identifier" hint))))

  (defun hints-add (elements)
    "Adds hints on elements"
    (ps:let* ((elements-length (length elements))
              (hints (hints-generate elements-length)))
      (ps:chain |json|
                (stringify
                 (loop for i from 0 to (- elements-length 1)
                       when (and (nyxt/ps:element-drawable-p (elt elements i))
                                 (nyxt/ps:element-in-view-port-p (elt elements i)))
                       do (hint-add (elt elements i) (elt hints i))
                       when (or (and (nyxt/ps:element-drawable-p (elt elements i))
                                     (not (ps:lisp annotate-visible-only-p)))
                                (and (nyxt/ps:element-drawable-p (elt elements i))
                                     (nyxt/ps:element-in-view-port-p (elt elements i))))
                       collect (object-create (elt elements i) (elt hints i)))))))

  (defun hints-determine-chars-length (length alphabet)
    "Finds out how many chars long the hints must be"
    (floor (+ 1 (/ (log length) (log (ps:@ alphabet length))))))

  (defun hints-generate (length)
    "Generates hints that will appear on the elements"
    (ps:let ((alphabet (ps:lisp (hints-alphabet (current-mode 'web)))))
      (strings-generate length alphabet)))

  (defun strings-generate (length alphabet)
    "Generates strings of specified length"
    (ps:let ((chars-length (hints-determine-chars-length length alphabet)))
      (ps:let ((minimum (1+ (ps:chain -math (pow (ps:@ alphabet length)
                                                 (- chars-length 1))))))
        (loop for i from minimum to (+ minimum length)
              collect (string-generate i alphabet)))))

  (defun string-generate (n alphabet)
    "Generates a string from a number"
    (ps:let ((alphabet-length (ps:@ alphabet length)))
      (if (>= n 0)
          (+ (string-generate (floor (- (/ n alphabet-length) 1)) alphabet)
             (aref alphabet (rem n alphabet-length))) "")))

  (add-stylesheet)
  (hints-add (nyxt/ps:qsa document (list "a" "button" "input" "textarea" "img"))))

(define-parenscript remove-element-hints ()
  (defun hints-remove-all ()
    "Removes all the elements"
    (ps:dolist (element (nyxt/ps:qsa document ".nyxt-hint"))
      (ps:chain element (remove))))
  (hints-remove-all))

(define-parenscript click-element (&key nyxt-identifier)
  (ps:chain (nyxt/ps:qs document (ps:lisp (format nil "[nyxt-identifier=\"~a\"]" nyxt-identifier))) (click)))

(define-parenscript highlight-selected-hint (&key link-hint scroll)

  (defun update-hints ()
    (ps:let* ((new-element (nyxt/ps:qs document (ps:lisp (format nil "#nyxt-hint-~a" (identifier link-hint))))))
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
           (lambda (hint)
             (str:starts-with-p input (hint hint) :ignore-case t))
           suggestions
           :key #'prompter:value)
        (append matching-hints other-hints))))
   (prompter:follow-mode-functions (lambda (suggestion)
                                     (highlight-selected-hint :link-hint suggestion)))))

(serapeum:export-always 'query-hints)
(defun query-hints (prompt function &key multi-selection-p annotate-visible-only-p)
  (let* ((buffer (current-buffer)))
    (let ((result (prompt
                   :prompt prompt
                   :extra-modes '(element-hint-mode)
                   :history nil
                   :sources
                   (make-instance
                    'hint-source
                    :multi-selection-p multi-selection-p
                    :constructor
                    (elements-from-json (add-element-hints
                                         :annotate-visible-only-p annotate-visible-only-p)))
                   :after-destructor
                   (lambda ()
                     (with-current-buffer buffer
                       (remove-element-hints))))))
      (funcall function result))))

(defun elements-from-json (elements-json)
  (loop for element in (cl-json:decode-json-from-string elements-json)
        collect (str:string-case (alex:assoc-value element :type)
                  ("link"
                   (make-instance 'link-hint
                                  :hint (alex:assoc-value element :hint)
                                  :identifier (alex:assoc-value element :hint)
                                  :url (quri:uri (alex:assoc-value element :href))
                                  :body (plump:text (plump:parse (alex:assoc-value element :body)))))
                  ("button"
                   (make-instance 'button-hint
                                  :identifier (alex:assoc-value element :identifier)
                                  :hint (alex:assoc-value element :hint)
                                  :body (plump:text (plump:parse (alex:assoc-value element :body)))))
                  ("input"
                   (make-instance 'input-hint
                                  :identifier (alex:assoc-value element :identifier)
                                  :hint (alex:assoc-value element :hint)
                                  :placeholder-text (alex:assoc-value element :placeholder)))
                  ("textarea"
                   (make-instance 'textarea-hint
                                  :identifier (alex:assoc-value element :identifier)
                                  :hint (alex:assoc-value element :hint)
                                  :placeholder-text (alex:assoc-value element :placeholder)))
                  ("img"
                   (make-instance 'image-hint
                                  :identifier (alex:assoc-value element :identifier)
                                  :url (quri:uri (alex:assoc-value element :src))
                                  :alt (alex:assoc-value element :alt)
                                  :hint (alex:assoc-value element :hint)))
                  ("clickable"
                   (make-instance 'clickable-hint
                                  :identifier (alex:assoc-value element :identifier)
                                  :hint (alex:assoc-value element :hint)
                                  :body (plump:text (plump:parse (alex:assoc-value element :body)))))
                  ("focusable"
                   (make-instance 'focusable-hint
                                  :identifier (alex:assoc-value element :identifier)
                                  :hint (alex:assoc-value element :hint))))))

(define-class hint ()
  ((hint "")
   (identifier "")
   (body ""
         :documentation "The body of the anchor tag."))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(defmethod prompter:object-attributes ((hint hint))
  `(("Hint" ,(hint hint))
    ("Body" ,(body hint))
    ("URL" ,(if (slot-exists-p hint 'url) (render-url (url hint)) ""))))

(define-class clickable-hint (hint) ())

(define-class focusable-hint (hint) ())

(define-class button-hint (clickable-hint) ())

(define-class link-hint (hint)
  ((url (quri:uri "")))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(define-class input-hint (focusable-hint)
  ((placeholder-text "" :documentation "The placeholder text of the input element.
I.e. the grey text initially seen in it.")
   (body "Input Area"))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(define-class textarea-hint (focusable-hint)
  ((placeholder-text "" :documentation "The placeholder text of the textarea.
I.e. the grey text initially seen in it.")
   (body "Text Area"))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(define-class image-hint (link-hint)
  ((alt "" :documentation "Alternative text for the image.")
   (body "Image"))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(defmethod %follow-hint ((link-hint link-hint))
  (buffer-load (url link-hint)))

(defmethod %follow-hint ((clickable-hint clickable-hint))
  (click-element :nyxt-identifier (identifier clickable-hint)))

(defmethod %follow-hint ((focusable-hint focusable-hint))
  (focus-element ()
    (nyxt/ps:qs document (ps:lisp (format nil "[nyxt-identifier=\"~a\"]" (identifier focusable-hint))))))

(defmethod %follow-hint-new-buffer-focus ((link-hint link-hint) &optional parent-buffer)
  (make-buffer-focus :url (url link-hint)
                     :parent-buffer parent-buffer
                     :nosave-buffer-p (nosave-buffer-p parent-buffer)))

(defmethod %follow-hint-new-buffer-focus ((hint hint) &optional parent-buffer)
  (declare (ignore parent-buffer))
  (echo "Unsupported operation for hint: can't open in new buffer."))

(defmethod %follow-hint-new-buffer ((link-hint link-hint) &optional parent-buffer)
  (make-buffer :url (url link-hint) :parent-buffer parent-buffer))

(defmethod %follow-hint-new-buffer ((hint hint) &optional parent-buffer)
  (declare (ignore parent-buffer))
  (echo "Unsupported operation for hint: can't open in new buffer."))

(defmethod %follow-hint-nosave-buffer-focus ((link-hint link-hint))
  (make-buffer-focus :url (url link-hint) :nosave-buffer-p t))

(defmethod %follow-hint-nosave-buffer-focus ((hint hint))
  (echo "Unsupported operation for hint: can't open in new buffer."))

(defmethod %follow-hint-nosave-buffer ((link-hint link-hint))
  (make-nosave-buffer :url (url link-hint)))

(defmethod %follow-hint-nosave-buffer ((hint hint))
  (echo "Unsupported operation for hint: can't open in new buffer."))

(defmethod %copy-hint-url ((link-hint link-hint))
  (trivial-clipboard:text (render-url (url link-hint))))

(defmethod %copy-hint-url ((hint hint))
  (echo "Unsupported operation for hint: can't copy URL."))

(defun prompt-buffer-selection-highlight-hint (&key suggestions scroll follow
                                                 (prompt-buffer (current-prompt-buffer))
                                                 (buffer (current-buffer)))
  (let ((hint (flet ((hintp (hint-suggestion)
                       (if (typep hint-suggestion '(or link-hint button-hint search-match))
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
            (highlight-selected-hint :link-hint hint
                                     :scroll scroll))
          (remove-focus)))))

(define-command follow-hint (&key annotate-visible-only-p)
  "Show a set of element hints, and go to the user inputted one in the current
buffer."
  (query-hints "Go to element" (lambda (results) (%follow-hint (first results)))
               :annotate-visible-only-p annotate-visible-only-p))

(define-command follow-hint-new-buffer (&key annotate-visible-only-p)
  "Show a set of element hints, and open the user inputted one in a new
buffer (not set to visible active buffer)."
  (let ((buffer (current-buffer)))
    (query-hints "Open element in new buffer"
                 (lambda (result) (mapcar (alex:rcurry #'%follow-hint-new-buffer buffer)
                                          result))
                 :multi-selection-p t
                 :annotate-visible-only-p annotate-visible-only-p)))

(define-command follow-hint-new-buffer-focus (&key annotate-visible-only-p)
  "Show a set of element hints, and open the user inputted one in a new
visible active buffer."
  (let ((buffer (current-buffer)))
    (query-hints "Go to element in new buffer"
                 (lambda (result)
                   (%follow-hint-new-buffer-focus (first result) buffer)
                   (mapcar (alex:rcurry #'%follow-hint-new-buffer buffer)
                           (rest result)))
                 :multi-selection-p t
                 :annotate-visible-only-p annotate-visible-only-p)))

(define-command follow-hint-nosave-buffer (&key annotate-visible-only-p)
  "Show a set of element hints, and open the user inputted one in a new
nosave buffer (not set to visible active buffer)."
  (query-hints "Open element in new buffer"
               (lambda (result) (mapcar #'%follow-hint-nosave-buffer result))
               :multi-selection-p t
               :annotate-visible-only-p annotate-visible-only-p))

(define-command follow-hint-nosave-buffer-focus (&key annotate-visible-only-p)
  "Show a set of element hints, and open the user inputted one in a new
visible nosave active buffer."
  (query-hints "Go to element in new buffer"
               (lambda (result)
                 (%follow-hint-nosave-buffer-focus (first result))
                 (mapcar #'%follow-hint-nosave-buffer (rest result)))
               :multi-selection-p t
               :annotate-visible-only-p annotate-visible-only-p))

(define-command copy-hint-url (&key annotate-visible-only-p)
  "Show a set of element hints, and copy the URL of the user-inputted one."
  (query-hints "Copy element URL" (lambda (result)  (%copy-hint-url (first result)))
               :annotate-visible-only-p annotate-visible-only-p))

(define-command bookmark-hint (&key annotate-visible-only-p)
  "Show link hints on screen, and allow the user to bookmark one"
  (query-hints "Bookmark hint"
               (lambda (result)
                 (let ((url (url (first result))))
                   (bookmark-url :url url)))
               :multi-selection-p t
               :annotate-visible-only-p annotate-visible-only-p))

(define-command download-hint-url (&key annotate-visible-only-p)
  "Download the file under the URL(s) hinted by the user."
  (let ((buffer (current-buffer)))
    (query-hints "Download link URL"
                 (lambda (selected-links)
                   (loop for link in selected-links
                         ;; TODO: sleep should NOT be necessary to avoid breaking download
                         do (download buffer (url link))
                            (sleep 0.25)))
                 :multi-selection-p t
                 :annotate-visible-only-p annotate-visible-only-p)))
