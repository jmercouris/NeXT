;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)
;; TODO: which-key: List all bindings with some prefix.
;; TODO: Make sure it's easy enough to set global bindings.

(defmacro command-markup (fn &key (modes nil explicit-modes-p))
  "Print FN in HTML followed its bindings in parentheses."
  `(let ((spinneret:*suppress-inserted-spaces* t))
     (spinneret:with-html (:span
                           (:code (string-downcase (symbol-name ,fn)))
                           " ("
                           (:code (apply #'binding-keys ,fn (if ,explicit-modes-p
                                                                (list :modes ,modes)
                                                                '())))
                           ")"))))

(defmacro command-docstring-first-sentence (fn)
  "Print FN first docstring sentence in HTML."
  `(spinneret:with-html
     (:span
      (sera:ensure-suffix
       (or (first (ppcre:split "\\.\\s" (documentation ,fn 'function)))
           (error "Undocumented function ~a." ,fn))
       "."))))

(deftype nyxt-keymap-value ()
  '(or keymap:keymap function-symbol command))

(declaim (ftype (function (string &rest keymap:keymap)
                          keymap:keymap)
                make-keymap))
(export-always 'make-keymap)
(defun make-keymap (name &rest parents)
  "Like `keymap:make-keymap' but only allow binding function symbols, commands
or keymaps.

Example:

\(defvar *my-keymap* (make-keymap \"my-map\")
  \"My keymap.\")"
  (let ((keymap (apply #'keymap:make-keymap name parents)))
    (setf (keymap:bound-type keymap) 'nyxt-keymap-value)
    keymap))

(export-always 'current-keymaps)
(defun current-keymaps (&optional (buffer (let ((prompt-buffer (current-prompt-buffer)))
                                            (if (and prompt-buffer (ffi-focused-p prompt-buffer))
                                                prompt-buffer
                                                (current-buffer)))))
  "Return the list of `keymap' for the current buffer, ordered by priority.
If non-empty, return the result of BUFFER's `current-keymaps-hook' instead."
  (let ((keymaps
          (when buffer
            (cons (override-map buffer)
                  (delete nil (mapcar #'keymap (modes buffer)))))))
    (if (current-keymaps-hook buffer)
        (hooks:run-hook (current-keymaps-hook buffer) keymaps buffer)
        keymaps)))

(defun all-keymaps (&optional (window (current-window)))
  "Return all keymaps for WINDOW, including the buffer keymaps and the
prompt-buffer keymaps."
  (let ((buffer (active-buffer window)))
    (when buffer
      (apply #'append (list (override-map buffer))
             (mapcar
              (lambda (buffer-or-prompt-buffer)
                (delete nil (mapcar #'keymap (modes buffer-or-prompt-buffer))))
              (delete nil (list buffer (current-prompt-buffer))))))))

(declaim (ftype (function (keymap:key) boolean) pointer-event-p))
(defun pointer-event-p (key)
  "Return non-nil if key-chord is a pointer event, e.g. a mouton button click."
  (coerce (str:starts-with? "button" (keymap:key-value key))
          'boolean))

(defun keyspecs-without-keycode (keys)
  (keymap:keys->keyspecs
   (mapcar (lambda (key) (keymap:copy-key key :code 0))
           keys)))

(export-always 'keyspecs-with-optional-keycode)
(defun keyspecs-with-optional-keycode (keys) ; TODO: Remove "optional" from name.
  "Like `keymap:keyspecs' but displayes keys with keycodes like this:
KEYCODE-LESS-DISPLAY (KEYCODE-DISPLAY)."
  (let ((no-code-specs (keyspecs-without-keycode keys)))
    (if (find-if (complement #'zerop) keys :key #'keymap:key-code)
        (format nil "~s [~a]" no-code-specs (keymap:keys->keyspecs keys))
        (format nil "~s" no-code-specs))))

(export-always 'dispatch-input-event)
(defun dispatch-input-event (event buffer window printable-p)
  "Dispatch keys in WINDOW `key-stack'.
Return nil to forward to renderer or non-nil otherwise."
  (declare (ignore printable-p))
  (echo-dismiss) ; Clean up message-view on keypress.
  (with-accessors ((key-stack key-stack)) window
    (labels ((keyspecs (key &optional translated-key)
               (if translated-key
                   (let ((specs (keyspecs key))
                         (translated-specs (keyspecs translated-key)))
                     (if (string= specs translated-specs)
                         (format nil "~a" specs)
                         (format nil "~a (translated from ~a)"
                                 translated-specs
                                 specs)))
                   (keyspecs-with-optional-keycode key))))
      (when buffer
        (setf (last-event buffer) event))
      (cond
        ((ffi-generated-input-event-p window event)
         (log:debug "Forward generated event ~a" (keyspecs key-stack))
         (setf key-stack nil)
         nil)

        (t
         (multiple-value-bind (bound-function matching-keymap translated-key)
             (the nyxt-keymap-value
                  (keymap:lookup-key key-stack (current-keymaps)))
           (declare (ignore matching-keymap))
           (cond
             ((keymap:keymap-p bound-function)
              (echo "Pressed keys: ~a" (keyspecs-without-keycode key-stack))
              (log:debug "Prefix binding ~a" (keyspecs key-stack translated-key))
              t)

             ((typep bound-function '(or function-symbol command)) ; TODO: Only accept commands?
              (log:debug "Found key binding ~a to ~a" (keyspecs key-stack translated-key) bound-function)
              ;; We save the last key separately to keep it available to the
              ;; command even after key-stack has been reset in the other
              ;; thread.
              (setf (last-key window) (first key-stack))
              (unwind-protect
                   (run-async bound-function)
                ;; We must reset the key-stack on errors or else all subsequent
                ;; keypresses will keep triggering the same erroring command.
                (setf key-stack nil))
              t)

             ((or (and buffer (forward-input-events-p buffer))
                  (pointer-event-p (first (last key-stack))))
              (log:debug "Forward key ~s" (keyspecs key-stack))
              (setf key-stack nil)
              nil)

             ((and buffer (not (forward-input-events-p buffer)))
              ;; After checking `pointer-event-p', otherwise pointer events
              ;; might not be forwarded.
              (log:debug "Skipping input event key ~s" (keyspecs key-stack))
              (setf key-stack nil)
              t)

             (t
              (log:debug "Fallback forward key ~s" (keyspecs key-stack))
              (setf key-stack nil)
              nil))))))))
