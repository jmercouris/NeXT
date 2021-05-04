;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/tests/renderer)

(plan nil)

(class-star:define-class nyxt-user::test-data-profile (nyxt:data-profile)
  ((nyxt:name :initform "test"))
  (:documentation "Test profile."))

(defmethod nyxt:expand-data-path ((profile nyxt-user::test-data-profile) (path nyxt:data-path))
  "Don't persist data"
  nil)

(defmacro with-prompt-buffer-test (command &body body)
  (alexandria:with-gensyms (thread)
    `(let ((,thread (bt:make-thread (lambda () ,command))))
       (calispel:? (prompt-buffer-channel (current-window)))
       ,@body
       (return-selection)
       (bt:join-thread ,thread))))

;; TODO: Use `with-prompt-buffer-test'.
;; (with-prompt-buffer-test (set-url)
;;   (set-prompt-input (current-prompt-buffer) "foobar"))

(subtest "set-url"
  (setf nyxt::*headless-p* t)
  (nyxt:start :no-init t :no-auto-config t
              :socket "/tmp/nyxt-test.socket"
              :data-profile "test")
  (sleep 2)                             ; TODO: Wait properly.
  (let ((url "http://example.org/"))
    (let ((thread (bt:make-thread (lambda () (nyxt:set-url)))))
      (calispel:? (nyxt::prompt-buffer-channel (nyxt:current-window)))
      (nyxt::set-prompt-input (nyxt:current-prompt-buffer) url)
      (prompter:all-ready-p (nyxt:current-prompt-buffer))
      (nyxt/prompt-buffer-mode:return-selection)
      ;; (bt:join-thread thread) ; TODO: Make sure thread always returns.
      )
    (sleep 1)                           ; TODO: Wait properly.
    (prove:is (nyxt:render-url (nyxt:url (nyxt:current-buffer)))
              url)))

(finalize)
