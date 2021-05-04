;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(defmacro define-function (name args docstring &body body)
  "Eval ARGS and DOCSTRING then define function over the resulting lambda list
and string.
All ARGS are declared as `ignorable'."
  (let ((evaluated-args (eval args))
        (evaluated-docstring (eval docstring)))
    `(defun ,name ,evaluated-args
       ,evaluated-docstring
       (declare (ignorable ,@(set-difference (mapcar (lambda (arg) (if (listp arg) (first arg) arg))
                                                     evaluated-args)
                                             lambda-list-keywords)))
       ,@body)))

(sera:eval-always
  (define-class prompt-buffer (user-internal-buffer prompter:prompter)
    ((default-modes '(prompt-buffer-mode))
     (window nil
             :type (or null window)
             :export nil
             :documentation "The window in which the prompt buffer is showing.")
     (resumable-p t
                  :type boolean)
     (prompter:history (prompt-buffer-generic-history *browser*)
                       ;; No need to export or define the accessor since this is
                       ;; an override of the prompter slot.
                       :accessor nil
                       :export nil
                       :documentation "By default the prompter library creates a
new history for each new prompt buffer.  Here we set the history to be shared globally.")
     ;; TODO: Need a changed-callback?  Probably not, see `search-buffer'.  But
     ;; can we run the postprocessor without running the filter?
     (invisible-input-p nil
                        :documentation "If non-nil, input is replaced by a
placeholder character.  This is useful to conceal passwords.")
     (hide-suggestion-count-p nil
                              :documentation "Whether to show the number of
chosen suggestions inside brackets.")
     ;; TODO: Need max-lines?
     ;; (max-lines 10
     ;;               :documentation "Max number of suggestion lines to show.
     ;; You will want edit this to match the changes done to `style'.")
     (hide-single-source-header-p nil
                                  :documentation "Hide source header when there is only one.")
     (style #.(cl-css:css
               '((* :font-family "monospace,monospace"
                    :font-size "14px"
                    :line-height "18px")
                 (body
                  :overflow "hidden"
                  :margin "0"
                  :padding "0")
                 ("#prompt-area"
                  :background-color "dimgray"
                  :display "grid"
                  :grid-template-columns "auto auto 1fr auto"
                  :width "100%"
                  :color "white")
                 ("#prompt"
                  :padding-left "10px"
                  :line-height "26px")
                 ("#prompt-extra"
                  :line-height "26px"
                  :padding-right "7px")
                 ("#prompt-modes"
                  :line-height "26px"
                  :padding-left "3px"
                  :padding-right "3px")
                 ("#input"
                  :border "none"
                  :outline "none"
                  :padding "3px"
                  :background-color "#E8E8E8"
                  :width "100%"
                  :autofocus "true")
                 (".source"
                  :margin-left "10px"
                  :margin-top "15px")
                 (".source-glyph"
                  :margin-right "3px")
                 (".source-name"
                  :color "white"
                  :padding-left "5px"
                  :line-height "24px"
                  :background-color "gray")
                 ("#suggestions"
                  :overflow-y "hidden"
                  :overflow-x "hidden"
                  :height "100%"
                  :width "100%")
                 (".source-content"
                  :margin-left "16px"
                  :background-color "#F7F7F7"
                  :width "100%"
                  :table-layout "fixed")
                 (".source-content td"
                  :white-space "nowrap"
                  :height "20px"
                  :overflow "auto")
                 (".source-content th"
                  :font-weight "normal"
                  :padding-left "3px"
                  :text-align "left"
                  :background-color "#E8E8E8")
                 (".source-content td::-webkit-scrollbar"
                  :display "none")
                 ("#selection"
                  :background-color "#37a8e4"
                  :color "white")
                 (.marked :background-color "darkgray"
                          :font-weight "bold"
                          :color "white")
                 (.selected :background-color "gray"
                            :color "white")))
            :documentation "The CSS applied to a prompt-buffer when it is set-up.")
     (override-map (make-keymap "override-map")
                   :type keymap:keymap
                   :documentation "Keymap that takes precedence over all modes' keymaps."))
    (:export-class-name-p t)
    (:export-accessor-names-p t)
    (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
    (:documentation "The prompt buffer is the interface for user interactions.
Each prompt spawns a new object: this makes it possible to nest prompts, such as
invoking `prompt-buffer-history'.

See `prompt' for how to invoke prompts.")))

(define-user-class prompt-buffer)

(defmethod initialize-instance :after ((prompt-buffer prompt-buffer) &key)
  (hooks:run-hook (prompt-buffer-make-hook *browser*) prompt-buffer)
  (initialize-modes prompt-buffer))

(export-always 'current-source)
(defun current-source (&optional (prompt-buffer (current-prompt-buffer)))
  (prompter:selected-source prompt-buffer))

(export-always 'current-suggestion-value)
(defun current-suggestion-value (&optional (prompt-buffer (current-prompt-buffer)))
  "Return selected prompt-buffer suggestion value.
Return source as second value.
To access the suggestion instead, see `prompter:selected-suggestion'."
  (multiple-value-bind (suggestion source)
      (prompter:selected-suggestion prompt-buffer)
    (values (when suggestion (prompter:value suggestion)) source)))

(defun show-prompt-buffer (prompt-buffer &key height)
  "Show the last active prompt-buffer, if any."
  ;; TODO: Add method that returns if there is only 1 source with no filter.
  (when prompt-buffer
    (push prompt-buffer (active-prompt-buffers (window prompt-buffer)))
    (calispel:! (prompt-buffer-channel (window prompt-buffer)) prompt-buffer)
    (prompt-render prompt-buffer)
    (run-thread
      (watch-prompt prompt-buffer))
    (ffi-window-set-prompt-buffer-height
     (window prompt-buffer)
     (or height
         (prompt-buffer-open-height (window prompt-buffer))))))

(export-always 'hide-prompt-buffer)
(defun hide-prompt-buffer (prompt-buffer &optional return-function)
  "Hide PROMPT-BUFFER, display next active one, and return PROMPT-BUFFER suggestion."
  ;; Note that PROMPT-BUFFER is not necessarily first in the list, e.g. a new
  ;; prompt-buffer was invoked before the old one reaches here.
  (alex:deletef (active-prompt-buffers (window prompt-buffer)) prompt-buffer)
  ;; The channel values are irrelevant, so is the element order:
  (calispel:? (prompt-buffer-channel (window prompt-buffer)) 0)
  (when (resumable-p prompt-buffer)
    (flet ((prompter= (prompter1 prompter2)
             (and (string= (prompter:prompt prompter1)
                           (prompter:prompt prompter2))
                  (string= (prompter:input prompter1)
                           (prompter:input prompter2)))))
      ;; Delete a previous, similar prompt, if any.
      (alex:deletef (old-prompt-buffers *browser*)
                    prompt-buffer
                    :test #'prompter=)
      (push prompt-buffer (old-prompt-buffers *browser*))))
  (if (active-prompt-buffers (window prompt-buffer))
      (let ((next-prompt-buffer (first (active-prompt-buffers (window prompt-buffer)))))
        (show-prompt-buffer next-prompt-buffer))
      (progn
        (ffi-window-set-prompt-buffer-height (window prompt-buffer) 0)))
  (funcall* return-function)
  ;; Destroy prompter last, or else `return-function' may not work.
  (prompter:destroy prompt-buffer))

(defun suggestion-and-mark-count (prompt-buffer suggestions marks
                                  &key multi-selection-p)
  (cond
    ((not suggestions)
     "")
    ((hide-suggestion-count-p prompt-buffer)
     "")
    (marks
     (format nil "[~a/~a]"
             (length marks)
             (length suggestions)))
    ((not marks)
     (format nil "[~a~a]"
             (if multi-selection-p
                 "0/"
                 "")
             (length suggestions)))))

(defun prompt-render-prompt (prompt-buffer)
  (let* ((suggestions (prompter:all-suggestions prompt-buffer))
         (marks (prompter:all-marks prompt-buffer)))
    (ffi-prompt-buffer-evaluate-javascript-async
     (window prompt-buffer)
     (ps:ps
       (setf (ps:chain document (get-element-by-id "prompt-extra") |innerHTML|)
             (ps:lisp
              (suggestion-and-mark-count
               prompt-buffer suggestions marks
               :multi-selection-p (some #'prompter:multi-selection-p
                                        (prompter:sources prompt-buffer)))))
       (setf (ps:chain document (get-element-by-id "prompt-modes") |innerHTML|)
             (ps:lisp
              (format nil "~{~a~^ ~}" (delete "prompt-buffer"
                                              (mapcar #'format-mode
                                                      (modes prompt-buffer))
                                              :test #'string=))))))))

(export 'prompt-render-suggestions)
(defmethod prompt-render-suggestions ((prompt-buffer prompt-buffer))
  "Refresh the rendering of the suggestion list.
This does not redraw the whole prompt buffer, unlike `prompt-render'."
  (let* ((sources (prompter:sources prompt-buffer))
         (current-source-index (position (current-source prompt-buffer) sources))
         (last-source-index (1- (length sources))))
    ;; TODO: Factor out attribute printing.
    (flet ((source->html (source)
             (markup:markup
              (:div :class "source"
                    (markup:raw
                     (markup:markup*
                      `(:div :class "source-name"
                             ,@(when (and (hide-single-source-header-p prompt-buffer)
                                          (sera:single sources))
                                 '(:hidden "true"))
                             (:span :class "source-glyph" "⛯")
                             ,(prompter:name source)
                             ,(if (prompter:hide-suggestion-count-p source)
                                  ""
                                  (suggestion-and-mark-count prompt-buffer
                                                             (prompter:suggestions source)
                                                             (prompter:marks source)
                                                             :multi-selection-p (prompter:multi-selection-p source))))))
                    (when (prompter:suggestions source)
                      (markup:raw
                       (markup:markup
                        (:table :class "source-content"
                                (markup:raw
                                 (markup:markup*
                                  `(:tr
                                    ,@(when (or (eq (prompter:hide-attribute-header-p source) :always)
                                                (and (eq (prompter:hide-attribute-header-p source) :single)
                                                     (sera:single (prompter:active-attributes-keys source))))
                                        '(:hidden "true"))
                                    ,@(loop for attribute-key in (prompter:active-attributes-keys source)
                                            collect `(:th ,attribute-key)))))
                                (loop ;; TODO: Only print as many lines as fit the height.  But how can we know in advance?
                                      ;; Maybe first make the table, then add the element one by one _if_ there are into view.
                                      with max-suggestion-count = 10
                                      repeat max-suggestion-count
                                      with cursor-index = (prompter:selected-suggestion-position prompt-buffer)
                                      for suggestion-index from (max 0 (- cursor-index (/ max-suggestion-count 2)))
                                      for suggestion in (nthcdr suggestion-index (prompter:suggestions source))
                                      collect (markup:markup
                                               (:tr :id (when (equal (list suggestion source)
                                                                     (multiple-value-list (prompter:selected-suggestion prompt-buffer)))
                                                          "selection")
                                                    :class (when (prompter:marked-p source (prompter:value suggestion))
                                                             "marked")
                                                    (loop for (nil attribute) in (prompter:active-attributes suggestion :source source)
                                                          collect (markup:markup (:td attribute))))))))))))))
      (ffi-prompt-buffer-evaluate-javascript-async
       (window prompt-buffer)
       (ps:ps
         (setf (ps:chain document (get-element-by-id "suggestions") |innerHTML|)
               (ps:lisp
                (sera:string-join (loop for i from current-source-index to last-source-index
                                        for source = (nth i sources)
                                        collect (source->html source))
                                  +newline+))))))
    (prompt-render-prompt prompt-buffer)))

(defun erase-document (prompt-buffer)
  (ffi-prompt-buffer-evaluate-javascript-async
   (window prompt-buffer)
   (ps:ps
     (ps:chain document (open))
     (ps:chain document (close)))))

(defun prompt-render-skeleton (prompt-buffer)
  (erase-document prompt-buffer)
  (ffi-prompt-buffer-evaluate-javascript-async
   (window prompt-buffer)
   (ps:ps (ps:chain document
                    (write
                     (ps:lisp (markup:markup
                               (:head (:style (style prompt-buffer)))
                               (:body
                                (:div :id "prompt-area"
                                      (:div :id "prompt" (prompter:prompt prompt-buffer))
                                      (:div :id "prompt-extra" "[?/?]")
                                      (:div (:input :type (if (invisible-input-p prompt-buffer)
                                                              "password"
                                                              "text")
                                                    :id "input"
                                                    :value (prompter:input prompt-buffer)))
                                      (:div :id "prompt-modes" ""))
                                (markup:raw
                                 (markup:markup*
                                  `(:div :id "suggestions"
                                         ,@(when (invisible-input-p prompt-buffer)
                                             '(:hidden "true")))))))))))))

(defun prompt-render-focus (prompt-buffer)
  (ffi-prompt-buffer-evaluate-javascript-async
   (window prompt-buffer)
   (ps:ps (ps:chain document
                    (get-element-by-id "input")
                    (focus)))))

(defmethod prompt-render ((prompt-buffer prompt-buffer)) ; TODO: Merge into `show-prompt-buffer'?
  (prompt-render-skeleton prompt-buffer)
  (prompt-render-focus prompt-buffer)
  (prompt-render-suggestions prompt-buffer))

(defun watch-prompt (prompt-buffer)
  "This blocks and updates the view."
  ;; TODO: Stop loop when prompt-buffer is no longer current.
  (sera:nlet maybe-update-view ((next-source (prompter:next-ready-p prompt-buffer)))
    (cond
      ;; Nothing to do:
      ((eq t next-source) t)
      ((null next-source) nil)
      (t ;; At least one source got updated.
       (prompt-render-suggestions prompt-buffer)
       (maybe-update-view (prompter:next-ready-p prompt-buffer))))))

(defun set-prompt-input (prompt-buffer input) ; TODO: Useless?
  "Set prompter's INPUT in PROMPT-BUFFER."
  (setf (prompter:input prompt-buffer)
        input))

(defun set-prompt-buffer-input (input)
  "Set HTML INPUT in PROMPT-BUFFER."
  (let ((window (current-window)))
    (when (first (active-prompt-buffers window))
      (ffi-prompt-buffer-evaluate-javascript
       window
       (ps:ps
         (setf (ps:chain document (get-element-by-id "input") value)
               (ps:lisp input)))))))

(export-always 'prompt)
(sera:eval-always
  (define-function prompt (append
                           '(&rest args)
                           `(&key ,@(delete-duplicates
                                     (append
                                      (public-initargs 'prompt-buffer)
                                      (public-initargs 'prompter:prompter)))))
      "Open the prompt buffer, ready for user input.
PROMPTER and PROMPT-BUFFER are plists of keyword arguments passed to the
prompt-buffer constructor.

Example use:

\(prompt
  :prompt \"Test prompt\"
  :sources (list (make-instance 'prompter:source :filter #'my-suggestion-filter)))

See the documentation of `prompt-buffer' to know more about the options."
    (let ((result-channel (make-channel 1))
          (interrupt-channel (make-channel 1)))
      (ffi-within-renderer-thread
       *browser*
       (lambda ()
         (let ((prompt-buffer (apply #'make-instance 'user-prompt-buffer
                                     (append args
                                             (list
                                              :window (current-window)
                                              :result-channel result-channel
                                              :interrupt-channel interrupt-channel)))))
           (show-prompt-buffer prompt-buffer))))
      ;; Wait until it's destroyed and get the selections from `return-selection'.
      (calispel:fair-alt
        ((calispel:? result-channel results)
         results)
        ((calispel:? interrupt-channel)
         (error 'nyxt-prompt-buffer-canceled))))))

(defmethod prompter:object-attributes ((prompt-buffer prompt-buffer))
  `(("Prompt" ,(prompter:prompt prompt-buffer))
    ("Input" ,(prompter:input prompt-buffer))))

(define-class resume-prompt-source (prompter:source)
  ((prompter:name "Resume prompters")
   (prompter:constructor (old-prompt-buffers *browser*))
   ;; TODO: Remove duplicates.
   ;; TODO: History?
   ))

(define-command resume-prompt ()
  "Query an older prompt and resume it."
  (let ((old-prompt
          (first (prompt
                  :resumable-p nil
                  :prompt "Resume prompt session"
                  :sources (list (make-instance 'resume-prompt-source))))))
    (when old-prompt
      (prompter:resume old-prompt)
      (show-prompt-buffer old-prompt))))
