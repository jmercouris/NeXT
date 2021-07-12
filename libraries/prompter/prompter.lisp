;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :prompter)

(define-class sync-queue ()
  ((ready-sources '()
                  :type list
                  :export nil
                  :documentation
                  "List of ready sources.")
   (ready-channel (make-channel nil)
                  :type calispel:channel
                  :export nil
                  :documentation
                  "Communication channel with the `update' thread.")
   (sync-interrupt-channel (make-channel)
                           :type calispel:channel
                           :export nil
                           :documentation
                           "This channel can be used to stop the queue listening."))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "This object is used to memorize which sources are ready for a
given input.
A new object is created on every new input."))

(defvar *default-history-size* 1000)    ; TODO: Export?

(declaim (ftype (function (&key (:size fixnum)) containers:ring-buffer-reverse) make-history))
(defun make-history (&key (size *default-history-size*))
  "Return a new ring buffer."
  (the (values cl-containers:ring-buffer-reverse &optional)
       (containers:make-ring-buffer size :last-in-first-out)))

;; Same as `source' as to why we wrap in `eval-always'.
(sera:eval-always
  (define-class prompter ()
    ((input ""
            :accessor nil
            :reader input
            :documentation
            "User input.")

     (prompt ""
             :documentation
             "Prefix to the user input.")

     (sources '()
              :type (or null source (cons source))
              :documentation "List of `source's.
For convenience, if the initarg is a single source (that is, not inside a list),
it is automatically wrapped into a list upon initialization.
If the source is designated by a symbol, then it is automatically instantiated
with `make-instance'.")

     (selection '()
                ;; TODO: Index by (source-index suggestion-index) instead?
                ;; TODO: Use structure?
                :type list
                :export nil
                :reader selection
                :documentation "A pair of source and suggestion index.")

     (constructor nil
                  :type (or null function)
                  :documentation
                  "Function called with the prompter as argument.")

     (before-destructor nil
                        :type (or null function)
                        :documentation
                        "First function called with no parameters when calling the
`destroy' function over this prompter.
It's called before the sources are cleaned up.")
     (after-destructor nil
                       :type (or null function)
                       :documentation
                       "Last function called with no parameters when calling the
`destroy' function over this prompter.
It's called after the sources are cleaned up.

Note that the function is executed *before* performing an action.")

     (history (make-history)
              :type (or containers:ring-buffer-reverse null)
              :documentation
              "History of inputs for the prompter.
If nil, no history is used.")

     (result-channel (make-channel 1)
                     :type calispel:channel
                     :documentation
                     "Channel to which the selection is sent on exit.
Caller should also listen to `interrupt-channel' to know if the prompter was canceled.")
     (interrupt-channel (make-channel 1)
                        :type calispel:channel
                        :documentation
                        "Channel to which an arbitrary value is written on exit.
See also `result-channel'.")

     (sync-queue nil
                 :type (or null sync-queue)
                 :export nil
                 :documentation "See `sync-queue' class documentation.")

     (returned-p nil
                 :type boolean
                 :documentation
                 "Whether the prompter has been cancelled."))
    (:export-class-name-p t)
    (:export-accessor-names-p t)
    (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
    (:documentation "The prompter is an interface for user interactions.
A prompter object holds multiple sources (of type `source') which
contain a list of `suggestion's.

You can call `destroy' to call the registered termination functions of the
prompter and its sources.

Suggestions are computed asynchronously when `input' is updated.
Use `all-ready-p' and `next-ready-p' to know when the prompter is ready.
Sources suggestions can be retrieved, possibly partially, even when the
compution is not finished.")))

(defun update-sources (prompter &optional (text ""))
  (setf (sync-queue prompter) (make-instance 'sync-queue))
  (mapc (lambda (source) (update source text (ready-channel (sync-queue prompter))))
        (sources prompter)))

(defmethod initialize-instance :after ((prompter prompter) &key)
  (unless (stringp (prompt prompter))
    (setf (prompt prompter) (write-to-string (prompt prompter))))
  (unless (stringp (input prompter))
    (setf (input prompter) (write-to-string (input prompter))))
  (flet ((ensure-sources (specifiers)
           (mapcar (lambda (source-specifier)
                     (cond
                       ((source-p source-specifier)
                        source-specifier)
                       ((and (symbolp source-specifier)
                             (c2cl:subclassp source-specifier 'source))
                        (make-instance source-specifier))
                       (t (error "Bad source specifier ~s." source-specifier))))
                   (uiop:ensure-list specifiers))))
    (setf (sources prompter) (ensure-sources (sources prompter))))
  (select-first prompter)
  (maybe-funcall (constructor prompter) prompter)
  (update-sources prompter (input prompter))
  prompter)

(defmethod (setf selection) (value (prompter prompter))
  (setf (slot-value prompter 'selection) value)
  (let ((source (selected-source prompter)))
    (when (follow-p source)
      (if (< 0 (follow-delay source))
          (bt:make-thread
           (lambda ()
             (sleep (follow-delay source))
             (call-follow-mode-function prompter))
           :name "Prompter follow selection thread")
          (call-follow-mode-function prompter)))))

(export-always 'input)
(defmethod (setf input) (text (prompter prompter))
  "Update PROMPTER sources and return TEXT."
  (let ((old-input (slot-value prompter 'input)))
    (unless (string= old-input text)
      (setf (slot-value prompter 'input) text)
      (update-sources prompter text)
      (select-first prompter)))
  text)

(export-always 'destroy)
(defun destroy (prompter)
  "First call `before-destructor', then call all the source destructors, finally call
`after-destructor'.
Signal destruction by sending a value to PROMPTER's `interrupt-channel'."
  (maybe-funcall (before-destructor prompter))
  (mapc (lambda (source) (maybe-funcall (destructor source) prompter source))
        (sources prompter))
  (maybe-funcall (after-destructor prompter))
  ;; TODO: Interrupt before or after destructor?
  (calispel:! (sync-interrupt-channel (sync-queue prompter)) t)
  (calispel:! (interrupt-channel prompter) t))

(export-always 'call-follow-mode-function)
(defun call-follow-mode-function (prompter)
  (sera:and-let* ((action (first (follow-mode-functions (selected-source prompter))))
                  (suggestion (selected-suggestion prompter)))
    (funcall action (value suggestion))))

(defun select (prompter steps &key wrap-over-p)
  "Select suggestion by jumping STEPS forward.
If STEPS is 0, do nothing.
If STEPS is negative, go backward.
If the currently selected suggestion is the last one of the current source, go
to next source, or previous source if STEPS is negative."
  (unless (= 0 steps)
    (labels ((index->source (index &optional (sources (sources prompter)))
               (let ((limit (length (suggestions (first sources)))))
                 (if (or (< index limit)
                         ;; Can happen when INDEX is beyond the total length.
                         (null (rest sources)))
                     (first sources)
                     (index->source (- index limit) (rest sources)))))
             (source-length (sources)
               (reduce #'+ (mapcar (lambda (source) (length (suggestions source)))
                                   sources)))
             (previous-sources (source)
               (let ((current-source-position (position source (sources prompter))))
                 (subseq (sources prompter) 0 current-source-position))))
      (let ((limit (source-length (sources prompter))))
        (declare (type unsigned-byte limit))
        (unless (= 0 limit)
          (let* ((previous-sources (previous-sources (first (selection prompter))))
                 (index (+ (second (selection prompter))
                           (source-length previous-sources)))
                 (new-index (+ index steps)))
            (setf new-index
                  (if wrap-over-p
                      (mod new-index limit)
                      (alex:clamp new-index 0 (max (1- limit) 0))))
            (let* ((new-source (index->source new-index))
                   (relative-index (- new-index
                                      (source-length (previous-sources new-source)))))
              (declare (type unsigned-byte relative-index))
              (setf (selection prompter)
                    (list new-source relative-index)))))))))

(export-always 'select-next)
(defun select-next (prompter &optional (steps 1))
  "Select element by jumping STEPS forward.
If STEPS is 0, do nothing.
If STEPS is negative, go backward."
  (select prompter steps))

(export-always 'select-previous)
(defun select-previous (prompter &optional (steps 1))
  "Select element by jumping STEPS forward.
If STEPS is 0, do nothing.
If STEPS is negative, go forward."
  (select prompter (- steps)))

(defun empty-source-p (source)
  (not (suggestions source)))

(export-always 'select-next-source)
(defun select-next-source (prompter &optional (steps 1))
  "Jumping STEPS non-empty source forward and select first suggestion.
If STEPS is 0, do nothing.
If STEPS is negative, go backward and select last suggestion."
  (unless (= 0 steps)
    (sera:and-let* ((nonempty-sources (remove-if #'empty-source-p (sources prompter)))
                    (source-index (or (position (selected-source prompter)
                                                nonempty-sources)
                                      0))
                    (new-source (nth (alex:clamp (+ steps source-index) 0 (1- (length (sources prompter))))
                                     nonempty-sources))
                    (suggestion-index (if (< 0 steps)
                                          0
                                          (1- (length (suggestions new-source))))))
      (setf (selection prompter)
            (list new-source suggestion-index)))))

(export-always 'select-previous-source)
(defun select-previous-source (prompter &optional (steps 1))
  "Jumping STEPS source backward and select last suggestion.
If STEPS is 0, do nothing.
If STEPS is negative, go forward and selection first suggestion."
  (unless (= 0 steps)
    (select-next-source prompter (- steps))))

(defun nonempty-source-p (source)
  (suggestions source))

(export-always 'select-first)
(defun select-first (prompter)
  "Select first element.
Empty sources are skipped."
  (let ((first-non-empty-source
          (or (find-if #'nonempty-source-p (sources prompter))
              (first (sources prompter)))))
    (setf (selection prompter)
          (list first-non-empty-source 0))))

(export-always 'select-last)
(defun select-last (prompter)
  "Select last element."
  (let ((last-non-empty-source
          (or (find #'nonempty-source-p (sources prompter)
                    :from-end t)
              (first (last (sources prompter))))))
    (setf (selection prompter)
          (list last-non-empty-source
                (1- (length (suggestions last-non-empty-source)))))))

(export-always 'toggle-mark)
(defun toggle-mark (prompter)
  (when (multi-selection-p (selected-source prompter))
    (multiple-value-bind (suggestion source)
        (selected-suggestion prompter)
      (with-accessors ((marks marks)) source
        (let ((value (value suggestion)))
          (if (find value marks)
              (setf marks (delete value marks))
              (push value marks)))))))

(export-always 'mark-all)
(defun mark-all (prompter)
  (let ((source (selected-source prompter)))
    (when (multi-selection-p source)
      (alex:unionf (marks source)
                   (mapcar #'value (suggestions source))))))

(export-always 'unmark-all)
(defun unmark-all (prompter)
  (let ((source (selected-source prompter)))
    (when (multi-selection-p source)
      (with-accessors ((marks marks)
                       (suggestions suggestions))
          source
        (setf marks
              (set-difference marks
                              (mapcar #'value suggestions)))))))

(export-always 'toggle-mark-all)
(defun toggle-mark-all (prompter)
  (let ((source (selected-source prompter)))
    (when (multi-selection-p source)
      (with-accessors ((suggestions suggestions)
                       (marks marks))
          source
        (let ((suggestion-values (mapcar #'value suggestions)))
          (setf marks
                (cond
                  ((subsetp marks suggestion-values)
                   (set-difference suggestion-values marks))
                  ((subsetp suggestion-values marks)
                   (set-difference marks suggestion-values))
                  (t ; When the intersection of suggestion-values and marks is non-trivial.
                   (set-difference
                    (union marks suggestion-values)
                    (intersection marks suggestion-values))))))))))

(defun resolve-selection (prompter)     ; TODO: Write tests for this!
  "Return the list of selected values.
If there is no marks, the current selection value is returned as a list of one element.
For instance, if the selected element value is NIL, this returns '(NIL).
If there is no element, NIL is returned."
  (or (all-marks prompter)
      (mapcar #'value (uiop:ensure-list (selected-suggestion prompter)))))

(export-always 'actions)
(defun actions (prompter)
  "Return the list of contextual actions.
Without marks, it's the list of actions for the current source.
With marks, it's the intersection of the actions of the sources that contain the
marked elements."
  (alex:if-let ((marked-sources
                 (remove-if (complement #'marks) (sources prompter))))
    (reduce #'intersection (mapcar (lambda (source)
                                     (slot-value source 'actions))
                                   marked-sources))
    (slot-value (selected-source prompter) 'actions)))

(defun history-pushnew (history element &key (test #'equal) )
  (alex:when-let ((previous-element-index (containers:element-position
                                     history
                                     element
                                     :test test)))
    (containers:delete-item-at history
                               previous-element-index))
  (containers:insert-item history element))


(defun add-input-to-history (prompter)
  "Add PROMPTER's current input to its history, if any.
If input is already in history, move to first position."
  (unless (or (null (history prompter))
              (str:empty? (input prompter)))
    (history-pushnew (history prompter)
                     (input prompter))))

(export-always 'return-selection)
(defun return-selection (prompter &optional (action (default-action prompter)))
  "Call action over selection and send the results to PROMPTER's `result-channel'.
The selection is the collection of marked suggestions across all sources.
If there is no marked suggestion, send the currently selected suggestion
instead."
  (unless action
    (setf action #'identity))
  (setf (returned-p prompter) t)
  (add-input-to-history prompter)
  (alex:if-let ((selection-values (resolve-selection prompter)))
    (let ((action-result (funcall action selection-values)))
      (calispel:! (result-channel prompter) action-result))
    (destroy prompter)))

(export-always 'toggle-follow)
(defun toggle-follow (prompter &optional (source (selected-source prompter)))
  "Toggle `follow-p' in SOURCE."
  (setf (follow-p source) (not (follow-p source))))

(export-always 'next-ready-p)
(defun next-ready-p (prompter &optional timeout)
  "Block and return next PROMPTER ready source.
It's the next source that's done updating.
If all sources are done, return t.
This is unblocked when the PROMPTER is `destroy'ed.

TIMEOUT is deprecated."
  (declare (ignore timeout)) ; Deprecated.
  (when prompter
    ;; We let-bind `sync-queue' here so that it remains the same object throughout
    ;; this function, since the slot is subject to be changed concurrently when
    ;; the input is edited.
    (alex:if-let ((sync-queue (sync-queue prompter)))
      (if (= (length (ready-sources sync-queue))
             (length (sources prompter)))
          t
          (progn
            (log:info "LISTEN" prompter (ready-channel sync-queue))
            (calispel:fair-alt
              ((calispel:? (ready-channel sync-queue) next-source)
               (log:info "LISTENED" next-source)
               (cond
                 ((null next-source)
                  nil)
                 (t
                  (push next-source (ready-sources sync-queue))
                  ;; Update selection when update is done:
                  (select-first prompter)
                  next-source)))
              ((calispel:? (sync-interrupt-channel sync-queue))
               (log:info "LISTENING INTERRUPTED")
               nil))))
      ;; No sync-queue if no input was ever set.
      t)))

(export-always 'all-ready-p)
(defun all-ready-p (prompter &optional timeout)
  "Return non-nil when all prompter sources are ready.
After timeout has elapsed for one source, return nil."
  (sera:nlet check ((next-source (next-ready-p prompter timeout)))
    (cond
      ((eq t next-source)
       t)
      ((null next-source)
       nil)
      (t
       (check (next-ready-p prompter timeout))))))

(export-always 'make)
(define-function make
    (append '(&rest args)
            `(&key ,@(public-initargs 'prompter)))
  "Return prompter object."
  (apply #'make-instance 'prompter args))

(export-always 'selected-source)
(defun selected-source (prompter)
  (first (selection prompter)))

(export-always 'selected-suggestion)
(defun selected-suggestion (prompter)
  "Return selected prompt-buffer suggestion.
Return source as second value."
  (let* ((source (first (selection prompter))))
    (values (nth (second (selection prompter)) (suggestions source)) source)))

(export-always 'selected-suggestion-position)
(defun selected-suggestion-position (prompter)
  "Return selected prompt-buffer suggestion position among current source
suggestions."
  (second (selection prompter)))

(export-always 'all-marks)
(defun all-marks (prompter)
  "Return the list of the marked suggestions in the prompter."
  (alex:mappend #'marks (sources prompter)))

(export-always 'all-suggestions)
(defun all-suggestions (prompter)
  "Return the list of the suggestions in the prompter."
  (alex:mappend #'suggestions (sources prompter)))

(defun default-action (prompter)
  (first (actions prompter)))

(export-always 'resume)
(defun resume (prompter)
  "Calls each source `resumer' function over the source.
This is meant to be called when a prompter is resumed."
  (mapc (lambda (source)
          (maybe-funcall (resumer source) source))
        (sources prompter)))
