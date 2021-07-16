;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/repl-mode)

(define-mode repl-mode ()
  "Mode for interacting with the REPL."
  ((rememberable-p nil)
   (keymap-scheme
    (define-scheme "repl"
      scheme:cua
      (list
       "return" 'return-input)
      scheme:emacs
      (list)))
   (style (cl-css:css
           '((* :font-family "monospace,monospace")
             (body :margin-right "0")
             ("#container" :display "flex"
                           :flex-flow "column"
                           :height "100%")
             ("#input" :display "grid"
                       :grid-template-columns "auto 1fr"
                       :width "100%"
                       :padding 0
                       :margin 0
                       :background-color "dimgray")
             ("#input-buffer" :width "100%"
                              :border "none"
                              :outline "none"
                              :padding "3px"
                              :background-color "gainsboro"
                              :autofocus "true")
             ("#evaluation-history"
              :font-size "12px"
              :flex-grow "1"
              :overflow-y "auto"
              :overflow-x "auto")
             ("#prompt" :padding-right "4px"
                        :padding-left "4px"
                        :line-height "30px"
                        :color "white")
             (ul :list-style "none"
                 :padding "0"
                 :margin "0")
             (li :padding "2px")))
          :documentation "The CSS applied to a REPL when it is set-up.")
   (evaluation-history (list))
   (constructor
    (lambda (mode)
      (initialize-display mode)
      (update-evaluation-history-display mode)))))

(define-command return-input (&optional (repl (current-mode 'repl)))
  "Return inputted text."
  (pflet ((input-text ()
           (ps:chain document (get-element-by-id "input-buffer") value)))
    (let ((input (input-text)))
      (add-object-to-evaluation-history repl (format nil "> ~a" input))
      (dolist (result (nyxt::evaluate input))
        (add-object-to-evaluation-history repl result))
      (reset-input repl)
      (update-evaluation-history-display repl))))

(define-command reset-input (&optional (repl (current-mode 'repl)))
  "Clear the inputted text."
  (with-current-buffer (buffer repl)
    (pflet ((clear-input-buffer-text ()
           (setf (ps:chain document (get-element-by-id "input-buffer") value) "")))
      (clear-input-buffer-text))))

(defmethod initialize-display ((repl repl-mode))
  (let* ((content (spinneret:with-html-string
                    (:head (:style (style repl)))
                    (:body
                     (:div :id "container"
                           (:div :id "evaluation-history" "")
                           (:div :id "input"
                                 (:span :id "prompt" ">")
                                 (:input :type "text" :id "input-buffer"))))))
         (insert-content (ps:ps (ps:chain document
                                          (write (ps:lisp content))))))
    (ffi-buffer-evaluate-javascript-async (buffer repl) insert-content)))

(defmethod add-object-to-evaluation-history ((repl repl-mode) item)
  (push item (evaluation-history repl)))

(defmethod update-evaluation-history-display ((repl repl-mode))
  (flet ((generate-evaluation-history-html (repl)
           (spinneret:with-html-string
            (:ul (loop for item in (reverse (evaluation-history repl))
                       collect (:li item))))))
    (ffi-buffer-evaluate-javascript-async
     (buffer repl)
     (ps:ps (setf (ps:chain document (get-element-by-id "evaluation-history") |innerHTML|)
                  (ps:lisp (generate-evaluation-history-html repl)))))))

(define-command-global lisp-repl ()
  "Show Lisp REPL."
  (let* ((repl-buffer (make-internal-buffer :title "*Lisp REPL*" :modes '(nyxt/repl-mode:repl-mode base-mode))))
    (set-current-buffer repl-buffer)
    repl-buffer))
