;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/editor-mode)

(define-mode plaintext-editor-mode (editor-mode)
  "Mode for basic plaintext editing."
  ((rememberable-p nil)
   (style (cl-css:css
           '(("body"
              :margin 0)
             ("#editor"
              :margin 0
              :position "absolute"
              :top "0"
              :right "0"
              :bottom "0"
              :left "0"
              :border "none"
              :outline "none"
              :padding "5px"
              :autofocus "true"))))
   (constructor
    (lambda (mode)
      (initialize-display mode)))))

(defmethod initialize-display ((editor plaintext-editor-mode))
  (let* ((content (spinneret:with-html-string
                    (:head (:style (style editor)))
                    (:body
                     (:textarea :id "editor" :name "editor" :autofocus t))))
         (insert-content (ps:ps (ps:chain document (write (ps:lisp content))))))
    (ffi-buffer-evaluate-javascript-async (buffer editor) insert-content)))

(defmethod set-content ((editor plaintext-editor-mode) content)
  (with-current-buffer (buffer editor)
    (pflet ((set-content (content)
                         (setf (ps:chain document (query-selector "#editor") value)
                               (ps:lisp content))))
      (set-content content))))

(defmethod get-content ((editor plaintext-editor-mode))
  (with-current-buffer (buffer editor)
    (pflet ((get-content () (ps:chain document (query-selector "#editor") value)))
      (get-content))))
