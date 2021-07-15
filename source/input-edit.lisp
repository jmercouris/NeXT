;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/input-edit-mode
  (:use :common-lisp :nyxt)
  (:import-from #:keymap #:define-key #:define-scheme)
  (:documentation "Mode for editing HTML input areas."))

(in-package :nyxt/input-edit-mode)

;;;; commands for navigating/editing input fields on HTML pages

(define-parenscript active-input-area-content ()
  (ps:chain document active-element value))

(define-parenscript set-active-input-area-content (content)
  (setf (ps:chain document active-element value) (ps:lisp content)))

(define-parenscript active-input-area-cursor ()
  (ps:chain document active-element selection-start))

(define-parenscript set-active-input-area-cursor (selection-start
                                                  selection-end)
  (ps:chain document active-element (set-selection-range
                                     (ps:lisp selection-start)
                                     (ps:lisp selection-end))))

(defmacro with-text-buffer ((buffer-name cursor-name
                             &optional initial-contents
                                       initial-cursor-position)
                            &body body)
  `(let ((,buffer-name (make-instance 'text-buffer:text-buffer))
         (,cursor-name (make-instance 'text-buffer:cursor)))
     (cluffer:attach-cursor ,cursor-name ,buffer-name)
     (when ,initial-contents
       (text-buffer::insert-string ,cursor-name ,initial-contents))
     (when ,initial-cursor-position
       (setf (cluffer:cursor-position ,cursor-name) (truncate ,initial-cursor-position)))
     ,@body))

(defmacro with-input-area ((contents cursor-position) &body body)
  `(let* ((,contents (active-input-area-content))
          (,cursor-position (truncate (active-input-area-cursor))))
     ,@body))

(define-command cursor-forwards ()
  "Move cursor forward by one element."
  (let* ((cursor-position (active-input-area-cursor)))
    (let ((new-position (1+ cursor-position)))
      (set-active-input-area-cursor new-position new-position))))

(define-command cursor-backwards ()
  "Move cursor backwards by one element."
  (let* ((cursor-position (truncate (active-input-area-cursor))))
    (let ((new-position (1- cursor-position)))
      (set-active-input-area-cursor new-position new-position))))

(define-command cursor-forwards-word ()
  "Move cursor forwards a word."
  (with-input-area (contents cursor-position)
    (with-text-buffer (text-buffer cursor contents cursor-position)
      (text-buffer::move-forward-word cursor
                                      :conservative-word-move
                                      (conservative-word-move (current-buffer)))
      (set-active-input-area-cursor (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor)))))

(define-command cursor-backwards-word ()
  "Move cursor backwards a word."
  (with-input-area (contents cursor-position)
    (with-text-buffer (text-buffer cursor contents cursor-position)
      (text-buffer::move-backward-word cursor
                                      :conservative-word-move
                                      (conservative-word-move (current-buffer)))
      (set-active-input-area-cursor (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor)))))

(define-command delete-forwards ()
  "Delete character after cursor."
  (with-input-area (contents cursor-position)
    (with-text-buffer (text-buffer cursor contents cursor-position)
      (text-buffer::delete-item-forward cursor)
      (set-active-input-area-content
       (text-buffer::string-representation text-buffer))
      (set-active-input-area-cursor (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor)))))

(define-command delete-backwards ()
  "Delete character before cursor."
  (with-input-area (contents cursor-position)
    (with-text-buffer (text-buffer cursor contents cursor-position)
      (text-buffer::delete-item-backward cursor)
      (set-active-input-area-content
       (text-buffer::string-representation text-buffer))
      (set-active-input-area-cursor (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor)))))

(define-command delete-backwards-word ()
  "Delete backwards a word."
  (with-input-area (contents cursor-position)
    (with-text-buffer (text-buffer cursor contents cursor-position)
      (text-buffer::delete-backward-word cursor)
      (set-active-input-area-content
       (text-buffer::string-representation text-buffer))
      (set-active-input-area-cursor (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor)))))

(define-command delete-forwards-word ()
  "Delete forwards a word."
  (with-input-area (contents cursor-position)
    (with-text-buffer (text-buffer cursor contents cursor-position)
      (text-buffer::delete-forward-word cursor)
      (set-active-input-area-content
       (text-buffer::string-representation text-buffer))
      (set-active-input-area-cursor (cluffer:cursor-position cursor)
                                    (cluffer:cursor-position cursor)))))

(define-mode input-edit-mode ()
  "Mode for editing input areas in HTML. Overrides many of the
bindings in other modes, so you will have to disable/enable it as
necessary."
  ((rememberable-p nil)
   (keymap-scheme
    (define-scheme "input-edit-mode"
      scheme:cua
      (list)
      scheme:emacs
      (list
       "C-f" 'cursor-forwards
       "C-b" 'cursor-backwards
       "M-f" 'cursor-forwards-word
       "M-b" 'cursor-backwards-word
       "C-d" 'delete-forwards
       "backspace" 'delete-backwards
       "M-backspace" 'delete-backwards-word
       "M-d" 'delete-forwards-word)))))
