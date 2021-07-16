;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :user-interface)

(defvar *id* 0 "Counter used to generate a unique ID.")

(defun unique-id ()
  (format nil "ui-element-~d" (incf *id*)))

(defclass ui-element ()
  ((id :accessor id)
   (buffer :accessor buffer :initarg :buffer
           :documentation "Buffer where element is drawn.")))

(defmethod initialize-instance :after ((element ui-element) &key)
  (setf (id element) (unique-id)))

(defmethod object-string ((element ui-element))
  (eval `(spinneret:with-html-string
           ,(object-expression element))))

(defmethod connect ((element ui-element) buffer)
  (setf (buffer element) buffer))

(defgeneric update (ui-element)
  (:documentation "Propagate changes to the buffer."))

(defclass button (ui-element)
  ((text :initform "" :initarg :text)
   (url :initform ""  :initarg :url)))

(defmethod (setf text) (text (button button))
  (setf (slot-value button 'text) text)
  (when (slot-boundp button 'buffer)
    (update button)))

(defmethod (setf url) (url (button button))
  (setf (slot-value button 'url) url)
  (when (slot-boundp button 'buffer)
    (update button)))

(defmethod text ((button button))
  (slot-value button 'text))

(defmethod url ((button button))
  (slot-value button 'url))

(defmethod object-expression ((button button))
  `(:a :id ,(id button) :class "button" :href ,(url button) ,(text button)))

(defclass paragraph (ui-element)
  ((text :initform "" :initarg :text)))

(defmethod (setf text) (text (paragraph paragraph))
  (setf (slot-value paragraph 'text) text)
  (when (slot-boundp paragraph 'buffer)
    (update paragraph)))

(defmethod object-expression ((paragraph paragraph))
  `(:p :id ,(id paragraph) ,(text paragraph)))

(defmethod text ((paragraph paragraph))
  (slot-value paragraph 'text))

(defclass progress-bar (ui-element)
  ((percentage :initform 0 :initarg :percentage
               :documentation "The percentage the progress bar is
filled up, use a number between 0 and 100.")))

(defmethod percentage ((progress-bar progress-bar))
  (slot-value progress-bar 'percentage))

(defmethod object-expression ((progress-bar progress-bar))
  `(:div :class "progress-bar-base"
         (:div :class "progress-bar-fill"
               :id ,(id progress-bar)
               ;; empty string to force markup to make closing :div tag
               "")))

(defmethod (setf percentage) (percentage (progress-bar progress-bar))
  (setf (slot-value progress-bar 'percentage) (format nil "~D%" (round percentage)))
  (when (slot-boundp progress-bar 'buffer)
    (update progress-bar)))
