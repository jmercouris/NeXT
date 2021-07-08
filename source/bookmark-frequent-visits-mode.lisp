;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(uiop:define-package :nyxt/new-test-mode
    (:use :common-lisp :trivia :nyxt :alexandria)
  (:documentation "Learn how to create a mode."))
(in-package :nyxt/new-test-mode)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (trivial-package-local-nicknames:add-package-local-nickname :alex :alexandria))

(define-mode new-test-mode ()
  "Learn how to create a mode."
  #+nil
  ((destructor
    (lambda (mode)
      (ffi-buffer-enable-sound (buffer mode) t)))
   (constructor
    (lambda (mode)
      (ffi-buffer-enable-sound (buffer mode) nil))))
  ((constructor
    (lambda (mode)
      (nyxt:on-signal-load-finished mode (url (current-buffer)))))))

#+nil
(defun bookmark-frequent-visit ()
  "Check if current URL is frequently visited and not included in the
bookmarks. If this is the case, prompt the user about bookmarking it."
  (labels ((bookmarked-url-p (url-address)
             "The local function bookmarked-url-p returns the URL
            address itself if it is new to the bookmark list or NIL if it is
            already there."
             (let ((bookmarks-address-list
                     (mapcar #'(lambda (e) (render-url (url e)))
                             (with-data-unsafe (bookmarks (bookmarks-path (current-buffer)))
                               bookmarks))))
               (if (member url-address bookmarks-address-list :test #'string=)
                   nil
                   url-address))))
    (let* ((history-entries
             (with-data-unsafe (history (history-path (current-buffer)))
               (mapcar #'htree:data (alex:hash-table-keys (htree:entries history)))))
           (current-url-history
             (find (url (current-buffer)) history-entries :test #'equalp :key #'url))
           (implicit-visits-value
             (nyxt::implicit-visits current-url-history))
           (current-url-address
             (render-url (url current-url-history)))
           (threshold 20))
      (if (and (> implicit-visits-value threshold)
               (bookmarked-url-p current-url-address))
          (if-confirm ("Bookmark ~a?" current-url-address)
                      (bookmark-url :url current-url-address))))))

#+nil
(define-mode bookmark-frequent-visits-mode ()
  "Bookmark current buffer URL if is frequently visited and not bookmarked yet."
  nil)

;; from documentation, the example to study:
#+nil
(define-mode my-mode ()
  "Dummy mode for the custom key bindings in `*my-keymap*'."
  ((keymap-schemes (keymap:make-scheme
                    scheme:emacs *my-keymap*
                    scheme:vi-normal *my-keymap*))))

#+nil
(defmethod nyxt:on-signal-load-finished ((mode web-mode) url)
  (add-url-to-history url (buffer mode) mode)
  (reset-page-zoom :buffer (buffer mode)
                   :ratio (current-zoom-ratio (buffer mode)))
  (with-data-unsafe (history (history-path (buffer mode)))
    (sera:and-let* ((owner (htree:owner history (id (buffer mode))))
                    (node (htree:current owner))
                    (data (htree:data node))
                    (scroll-position (nyxt::scroll-position data)))
      (setf (nyxt:document-scroll-position (buffer mode)) scroll-position)))
  url)

(defun print-no-nyxt ()
  (echo "XXXXXXXXXXXXXXXXX"))

(defun TWEAK-bookmark-frequent-visit ()
  "Check if current URL is frequently visited and not included in the
bookmarks. If this is the case, prompt the user about bookmarking it."
  (labels ((bookmarked-url-p (url-address)
             "The local function bookmarked-url-p returns the URL
            address itself if it is new to the bookmark list or NIL if it is
            already there."
             (let ((bookmarks-address-list
                     (mapcar #'(lambda (e) (render-url (url e)))
                             (with-data-unsafe (bookmarks (bookmarks-path (current-buffer)))
                               bookmarks))))
               (if (member url-address bookmarks-address-list :test #'string=)
                   nil
                   url-address))))
    (let* ((history-entries
             (with-data-unsafe (history (history-path (current-buffer)))
               (mapcar #'htree:data (alex:hash-table-keys (htree:entries history)))))
           (current-url-history
             (find (url (current-buffer)) history-entries :test #'equalp :key #'url))
           (implicit-visits-value
             (nyxt::implicit-visits current-url-history))
           (current-url-address
             (render-url (url current-url-history)))
           (threshold 20))
      (if (and (> implicit-visits-value threshold)
               (bookmarked-url-p current-url-address))
          (if-confirm ("Bookmark ~a?" current-url-address)
                      (bookmark-url :url current-url-address))))))

(defmethod nyxt:on-signal-load-finished ((mode new-test-mode) url)
  ;(add-url-to-history url (buffer mode) mode) ;chama uma função
  (print-no-nyxt)
  (TWEAK-bookmark-frequent-visit)
  url)
