;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(define-mode diff-mode ()
  "Diff mode is used to view the diffs between two buffers."
  ((rememberable-p nil)
   (buffer (error "Please supply a buffer.")
           :documentation "The buffer where the diff will be displayed.")
   (old-html :documentation "String html representation.")
   (new-html :documentation "String html representation.")
   (diff-html :documentation "String html representation of the computed diff
   between old-html and new-html slots.")
   (diff-style (cl-css:css
                '((".nyxt-diff-insert"
                   :text-decoration "none"
                   :background-color "#bbeabb")
                  ("ins.nyxt-diff-replace"
                   :text-decoration "none"
                   :background-color "#bbeabb")
                  (".nyxt-diff-delete"
                   :text-decoration "none"
                   :background-color "#efcbcf")
                  ("del.nyxt-diff-replace"
                   :text-decoration "none"
                   :background-color "#efcbcf")))
               :documentation "Diff colours for its visual representation.
They're based on the modus-operandi theme by Protesilaos Stavrou, which follows
the highest standard on accessibility.")
   (keymap-scheme (define-scheme "diff"
                    scheme:cua
                    (list "q" 'delete-current-buffer))
                  :type keymap:scheme)
   (destructor (lambda (instance) (nyxt::buffer-delete (buffer instance))))
   (constructor (lambda (instance)
                  (setf (diff-html instance)
                        (html-diff:html-diff (old-html instance)
                                             (new-html instance)
                                             :insert-class "nyxt-diff-insert"
                                             :delete-class "nyxt-diff-delete"
                                             :replace-class "nyxt-diff-replace"))
                  (nyxt::html-set (diff-html instance) (buffer instance))
                  (nyxt::html-set-style (diff-style instance) (buffer instance))
                  (ffi-buffer-evaluate-javascript
                   (buffer instance)
                   (ps:ps (setf (ps:chain document title) "*diff*")))))))

(define-command diff ()
  "Create a buffer showing a diff between 2 html documents."
  ;; users should be able to choose from buffers and/or files.  to be expanded
  ;; when file-manager-mode is fixed.
  (flet ((fetch-html-from-buffer (&key prompt)
           (ffi-buffer-get-document
            (prompt
             :prompt prompt
             :sources (list (make-instance 'user-buffer-source))))))
    ;; change buffer here, not at the constructor
    (set-current-buffer
     (diff-mode :old-html (fetch-html-from-buffer
                           :prompt "Old buffer")
                :new-html (fetch-html-from-buffer
                           :prompt "New buffer")
                :buffer (make-internal-buffer
                         ;; it's sensible to set the title here but it will be
                         ;; overridden anyway by html-set
                         :title "*diff*"
                         ;; only cua-mode keybindings work, why?
                         :modes '(base-mode))))))
