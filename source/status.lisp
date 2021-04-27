;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(export-always 'format-status-modes)
(defun format-status-modes (buffer window)
  (format nil "~:[~;⚠ nosave ~]~{~a~^ ~}"
          (nosave-buffer-p buffer)
          (mapcar (if (glyph-mode-presentation-p (status-buffer window))
                      #'glyph
                      #'format-mode)
                  (sera:filter #'visible-in-status-p (modes buffer)))))

(defun list-modes (buffer)
  (format nil "~{~a~^ ~}" (mapcar #'format-mode (modes buffer))))

(export-always 'format-status-buttons)
(defun format-status-buttons ()
  (markup:markup
   (:a :class "button" :title "Backwards" :href (lisp-url '(nyxt/web-mode:history-backwards)) "←")
   (:a :class "button" :title "Forwards" :href (lisp-url '(nyxt/web-mode:history-forwards)) "→")
   (:a :class "button" :title "Reload" :href (lisp-url '(nyxt:reload-current-buffer)) "↺")
   (:a :class "button" :title "Execute" :href (lisp-url '(nyxt:execute-command)) "⚙")
   (:a :class "button" :title "Buffers" :href (lisp-url '(nyxt::list-buffers)) "≡")))

(export-always 'format-status-load-status)
(defun format-status-load-status (buffer)
  (markup:markup
   (:span (if (and (web-buffer-p buffer)
                   (eq (load-status buffer) :loading))
              "Loading: " ""))))

(export-always 'format-status-url)
(defun format-status-url (buffer)
  (markup:markup
   (:a :class "button"
       :href (lisp-url '(nyxt:set-url))
       (format nil " ~a — ~a"
               (render-url (url buffer))
               (title buffer)))))

(export-always 'format-status-tabs)
(defun format-status-tabs ()
  (flet ((buffer-domains ()
           (remove-duplicates
            (remove nil
                    (mapcar #'(lambda (i) (quri:uri-domain (url i)))
                            (buffer-list :sort-by-time t)))
            :test #'equal)))
    (markup:markup
     (:span
      (loop for domain in (buffer-domains)
            collect (markup:markup
                     (:a :class "tab"
                         :href
                         (lisp-url `(nyxt::switch-buffer-or-query-domain ,domain)) domain)))))))

(defun format-status (window)
  (let ((buffer (current-buffer window)))
    (markup:markup
     (:div :id "container"
           (:div :id "controls"
                 (markup:raw (format-status-buttons)))
           (:div :class "arrow arrow-right" 
                 :style "background-color:rgb(80,80,80)" "")
           (:div :id "url"
                 (markup:raw
                  (format-status-load-status buffer)
                  (format-status-url buffer)))
           (:div :class "arrow arrow-right" 
                 :style "background-color:rgb(120,120,120)" "")
           (:div :id "tabs"
                 (markup:raw
                  (format-status-tabs)))
           (:div :class "arrow arrow-left"
                 :style "background-color:rgb(120,120,120)" "")
           (:div :id "modes"
                 :title (list-modes buffer)
                 (format-status-modes buffer window))))))
