;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/message-mode
  (:use :common-lisp :trivia :nyxt)
  (:documentation "Mode for messages and logs"))
(in-package :nyxt/message-mode)

(define-mode message-mode ()
  "Mode for log and message listing."
  ((rememberable-p nil)))

(define-command clear-messages ()
  "Clear the *Messages* buffer."
  (setf (slot-value *browser* 'messages-content) '())
  (echo "Messages cleared."))

(define-command-global list-messages ()
  "Show the *Messages* buffer."
  (with-current-html-buffer (buffer "*Messages*" 'nyxt/message-mode:message-mode)
    (spinneret:with-html-string
      (:style (style buffer))
      (:h1 "Messages")
      (:a :class "button"
          :href (lisp-url '(nyxt/message-mode::list-messages)) "Update")
      (:a :class "button"
          :href (lisp-url '(nyxt/message-mode:clear-messages)
                          '(nyxt/message-mode::list-messages))
          "Clear")
      (:ul
       (loop for message in (reverse (nyxt:messages-content *browser*))
             collect (:li message))))))
