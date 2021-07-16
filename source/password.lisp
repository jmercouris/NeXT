;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defun make-password-interface-user-classes ()
  "Define user classes so that users may apply define-configuration
macro to change slot values."
  (loop for interface in password:*interfaces* do
           (eval `(define-user-class ,(intern (symbol-name interface)
                                              (package-name (symbol-package interface)))))))

(make-password-interface-user-classes)

(defun make-password-interface ()
  "Return the instance of the first password interface among `password:*interfaces*'
for which the `executable' slot is non-nil."
  (some (lambda (interface)
          (let ((instance (make-instance (user-class-name interface))))
            (when (password:executable instance)
              instance)))
        password:*interfaces*))

(defvar password-source-actions
  (list (make-command clip-password (password-name)
          (let ((buffer (buffer (current-source)))
                (password-name (first password-name)))
            (password:clip-password (password-interface buffer) :password-name password-name)
            (echo "Password saved to clipboard for ~a seconds." (password:sleep-timer (password-interface buffer)))))
        (make-command clip-username (password-name)
          (let ((buffer (buffer (current-source)))
                (password-name (first password-name)))
            (if (password:clip-username (password-interface buffer) :password-name password-name)
                (echo "Username saved to clipboard.")
                (echo "No username found."))))))

(define-class password-source (prompter:source)
  ((prompter:name "Passwords")
   (buffer :accessor buffer :initarg :buffer)
   (password-instance :accessor password-instance :initarg :password-instance)
   (prompter:hide-attribute-header-p :single)
   (prompter:constructor
    (lambda (source)
      (password:list-passwords (password-instance source))))
   (prompter:actions password-source-actions)))

(defun password-debug-info ()
  (alex:when-let ((interface (password-interface (current-buffer))))
    (log:debug "Password interface ~a uses executable ~s."
               (class-name (class-of interface))
               (password:executable interface))))

(define-command save-new-password (&optional (buffer (current-buffer)))
  "Save password to password interface."
  (password-debug-info)
  (cond
    ((and (password-interface buffer)
          (has-method-p (password-interface buffer)
                        #'password:save-password))
     (let* ((password-name (first (prompt
                                   :prompt "Name for new password"
                                   :input (or (quri:uri-domain (url (current-buffer))) "")
                                   :sources (make-instance 'prompter:raw-source))))
            (new-password (first (prompt
                                  :invisible-input-p t
                                  :prompt "New password (leave empty to generate)"
                                  :sources (make-instance 'prompter:raw-source))))
            (username (first (prompt
                              :prompt "Username (can be empty)"
                              :sources (make-instance 'prompter:raw-source)))))
       (password:save-password (password-interface buffer)
                               :username username
                               :password-name password-name
                               :password new-password)))
    ((null (password-interface buffer))
     (echo-warning "No password manager found."))
    (t (echo-warning "Password manager ~s does not support saving passwords."
                     (string-downcase
                      (class-name (class-of (password-interface buffer))))))))

(defmethod password:complete-interface ((password-interface password:keepassxc-interface))
  (loop :until (and (password::password-file password-interface)
                    (string-equal "kdbx"
                                  (pathname-type (pathname (password::password-file
                                                            password-interface)))))
        :do (setf (password::password-file password-interface)
                  (namestring
                   (first (prompt
                           :prompt "Password file"
                           :sources (list (make-instance
                                           'user-file-source
                                           :filter-preprocessor
                                           (make-file-source-preprocessor
                                            (alex:disjoin (match-extension "kdbx")
                                                          #'uiop:directory-pathname-p)))))))))
  (loop :until (password:password-correct-p password-interface)
        :do (setf (password::master-password password-interface)
                  (first (prompt
                          :prompt "File password"
                          :sources (list (make-instance 'prompter:raw-source))
                          :invisible-input-p t)))))

(defmacro with-password (password-interface &body body)
  `(if (password:password-correct-p ,password-interface)
       ,@body
       (progn
         (password:complete-interface ,password-interface)
         ,@body)))

(define-command copy-password-prompt-details (&optional (buffer (current-buffer)))
  "Copy password prompting for all the details without suggestions."
  (password-debug-info)
  (if (password-interface buffer)
      (let* ((password-name (first (prompt
                                    :prompt "Name of password"
                                    :sources (make-instance 'prompter:raw-source))))
             (service (first (prompt
                              :prompt "Service"
                              :sources (make-instance 'prompter:raw-source)))))
        (handler-case
            (password:clip-password (password-interface buffer)
                                    :password-name password-name
                                    :service service)
          (error (c)
            (echo-warning "Error retrieving password: ~a" c))))
      (echo-warning "No password manager found.")))

(define-command copy-password (&optional (buffer (current-buffer)))
  "Query password and copy to clipboard."
  (password-debug-info)
  (if (password-interface buffer)
      (with-password (password-interface buffer)
        (first (prompt
                :prompt "Password"
                :input (quri:uri-domain (url buffer))
                :sources (list (make-instance
                                'password-source
                                :buffer buffer
                                :password-instance (password-interface buffer))))))
      (echo-warning "No password manager found.")))

(define-command copy-username (&optional (buffer (current-buffer)))
  "Query username and copy to clipboard."
  (password-debug-info)
  (if (password-interface buffer)
      (with-password (password-interface buffer)
        (prompt
         :prompt "Username"
         :input (quri:uri-domain (url buffer))
         :sources (list (make-instance 'password-source
                                       :buffer buffer
                                       :password-instance (password-interface buffer)
                                       :actions (sera:filter (sera:eqs 'clip-username)
                                                             password-source-actions
                                                             :key #'name)))))
      (echo-warning "No password manager found.")))
