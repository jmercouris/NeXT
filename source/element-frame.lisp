;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/web-mode)

(define-class html-element ()
  ((body ""))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(define-class link (html-element)
  ((url ""))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(define-class image (html-element)
  ((alt "" :documentation "Alternative text for the image.")
   (url ""))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(defun frame-element-select ()
  "Allow the user to draw a frame around elements to select them."
  (let ((overlay-style (cl-css:css
                        '(("#nyxt-overlay"
                           :position "fixed"
                           :top "0"
                           :left "0"
                           :right "0"
                           :bottom "0"
                           :background "rgba(0,0,0,0)"
                           :z-index #.(1- (expt 2 31))))))
        (selection-rectangle-style (cl-css:css
                                    '(("#nyxt-rectangle-selection"
                                       :position "absolute"
                                       :top "0"
                                       :left "0"
                                       :border "dotted 1px black"
                                       :background "rgba(0,0,0,0.05)"
                                       :z-index #.(1- (expt 2 30)))))))
    (pflet ((add-overlay (overlay-style selection-rectangle-style)
              "Add a selectable overlay to the screen."
              (defparameter selection
                (ps:create x1 0 y1 0
                           x2 0 y2 0
                           set1 false
                           set2 false))
              (defun add-stylesheet ()
                (unless (ps:chain document (get-element-by-id "nyxt-stylesheet"))
                  (ps:let ((style-element (ps:chain document (create-element "style"))))
                    (setf (ps:@ style-element id) "nyxt-stylesheet")
                    (ps:chain document head (append-child style-element)))))
              (defun add-style (style)
                (ps:let ((style-element (ps:chain document (get-element-by-id "nyxt-stylesheet"))))
                  (ps:chain style-element sheet (insert-rule style 0))))
              (defun add-overlay ()
                (ps:let ((element (ps:chain document (create-element "div"))))
                  (add-style (ps:lisp overlay-style))
                  (setf (ps:@ element id) "nyxt-overlay")
                  (ps:chain document body (append-child element))))
              (defun add-selection-rectangle ()
                (ps:let ((element (ps:chain document (create-element "div"))))
                  (add-style (ps:lisp selection-rectangle-style))
                  (setf (ps:@ element id) "nyxt-rectangle-selection")
                  (ps:chain document body (append-child element))))
              (defun update-selection-rectangle ()
                (ps:let ((element (ps:chain document (get-element-by-id "nyxt-rectangle-selection"))))
                  (setf (ps:@ element style left) (+ (ps:chain selection x1) "px"))
                  (setf (ps:@ element style top) (+ (ps:chain selection y1) "px"))
                  (setf (ps:@ element style width)
                        (+ (- (ps:chain selection x2)
                              (ps:chain selection x1))
                           "px"))
                  (setf (ps:@ element style height)
                        (+ (- (ps:chain selection y2)
                              (ps:chain selection y1))
                           "px"))))
              (defun add-listeners ()
                (setf (ps:chain document (get-element-by-id "nyxt-overlay") onmousemove)
                      (lambda (e)
                        (when (and (ps:chain selection set1)
                                   (not (ps:chain selection set2)))
                          (setf (ps:chain selection x2) (ps:chain e |pageX|))
                          (setf (ps:chain selection y2) (ps:chain e |pageY|))
                          (update-selection-rectangle))))
                (setf (ps:chain document (get-element-by-id "nyxt-overlay") onclick)
                      (lambda (e)
                        (if (not (ps:chain selection set1))
                            (progn
                              (setf (ps:chain selection x1) (ps:chain e |pageX|))
                              (setf (ps:chain selection y1) (ps:chain e |pageY|))
                              (setf (ps:chain selection set1) true))
                            (progn
                              (setf (ps:chain selection x2) (ps:chain e |pageX|))
                              (setf (ps:chain selection y2) (ps:chain e |pageY|))
                              (setf (ps:chain selection set2) true))))))
              (add-stylesheet)
              (add-overlay)
              (add-selection-rectangle)
              (add-listeners)))
      ;; Invoke the JavaScript asynchronously, otherwise this function is
      ;; blocking
      (bt:make-thread
       (lambda ()
         (add-overlay overlay-style selection-rectangle-style))))))

(defun frame-element-get-selection ()
  "Get the selected elements drawn by the user."
  (pflet ((get-selection ()
            (defun qsa (context selector)
              "Alias of document.querySelectorAll"
              (ps:chain context (query-selector-all selector)))
            (defun element-in-selection-p (selection element)
              "Determine if a element is bounded within a selection."
              (ps:let* ((element-rect (ps:chain element (get-bounding-client-rect)))
                        (offsetX (ps:chain window |pageXOffset|))
                        (offsetY (ps:chain window |pageYOffset|))
                        (element-left (+ (ps:chain element-rect left) offsetX))
                        (element-right (+ (ps:chain element-rect right) offsetX))
                        (element-top (+ (ps:chain element-rect top) offsetY))
                        (element-bottom (+ (ps:chain element-rect bottom) offsetY)))
                (if (and
                     (<= element-left (ps:chain selection x2))
                     (>= element-right (ps:chain selection x1))
                     (<= element-top (ps:chain selection y2))
                     (>= element-bottom (ps:chain selection y1)))
                    t nil)))
            (defun object-create (element)
              (cond ((equal "A" (ps:@ element tag-name))
                     (ps:create "type" "link" "href" (ps:@ element href) "body" (ps:@ element |innerHTML|)))
                    ((equal "IMG" (ps:@ element tag-name))
                     (ps:create "type" "img" "src" (ps:@ element src) "alt" (ps:@ element alt)))))
            (defun collect-selection (elements selection)
              "Collect elements within a selection"
              (loop for element in elements
                    when (element-in-selection-p selection element)
                      collect (object-create element)))
            (collect-selection (qsa document (list "a")) selection)))
    (loop for element in (get-selection)
          collect (str:string-case (gethash "type" element )
                    ("link"
                     (make-instance 'link
                                    :url (gethash "href" element )
                                    :body (plump:text (plump:parse (gethash "body" element )))))
                    ("img"
                     (make-instance 'image
                                    :url (gethash "src" element )
                                    :alt (gethash "alt" element )))))))

(defun frame-element-clear ()
  "Clear the selection frame created by the user."
  (pflet ((remove-overlay ()
            (ps:chain document (get-element-by-id "nyxt-rectangle-selection") (remove))
            (ps:chain document (get-element-by-id "nyxt-overlay") (remove))))
    (remove-overlay)))

(define-class frame-source (prompter:source)
  ((prompter:name "Selection Frame (confirm with C-s)")
   (buffer :accessor buffer :initarg :buffer)
   (channel :accessor channel :initarg :channel)
   (prompter:constructor (lambda (source)
                           (frame-element-select)
                           (calispel:? (channel source))))))

(define-command select-frame-new-buffer (&key (buffer (current-buffer)))
  "Select a frame and open the links in new buffers."
  (prompt
   :extra-modes '(frame-select-mode)
   :prompt "Open selected links in new buffers:"
   :sources (list (make-instance 'frame-source
                                 :buffer buffer
                                 :multi-selection-p t
                                 :channel (make-instance 'calispel:channel)
                                 :actions (list (make-command open-new-buffers (urls)
                                                  (mapcar (lambda (i) (make-buffer :url (quri:uri i))) urls)))))
   :after-destructor
   (lambda ()
     (with-current-buffer buffer
       (frame-element-clear)))))

(define-mode frame-select-mode ()
  "Mode for helping in frame selections."
  ((keymap-scheme
    (define-scheme "frame"
      scheme:cua
      (list
       "C-s" 'frame-source-confirm-selection)))))

(defun frame-source-confirm-selection ()
  (alex:when-let* ((frame-source 
                    (find-if 
                     (lambda (s)
                       (closer-mop:subclassp (class-of s)
                                             (find-class 'frame-source)))
                     (prompter:sources (current-prompt-buffer))))
                   (selection (remove-duplicates (mapcar #'url (frame-element-get-selection))
                                                 :test #'equal)))
    (calispel:! (channel frame-source) selection)))
