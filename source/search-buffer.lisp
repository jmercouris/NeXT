;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt/web-mode)

(define-parenscript query-buffer (&key query (case-sensitive-p nil))
  (defvar *identifier* 0)
  (defvar *matches* (array))
  (defvar *nodes* (ps:new (-Object)))
  (defvar *node-replacements* (array))

  (defun qs (context selector)
    "Alias of document.querySelector"
    (ps:chain context (query-selector selector)))

  (defun qsa (context selector)
    "Alias of document.querySelectorAll"
    (ps:chain context (query-selector-all selector)))

  (defun add-stylesheet ()
    (unless (qs document "#nyxt-stylesheet")
      (ps:try
       (ps:let* ((style-element (ps:chain document (create-element "style")))
                 (box-style (ps:lisp (box-style (current-mode 'web))))
                 (highlighted-style (ps:lisp (highlighted-box-style (current-mode 'web)))))
         (setf (ps:@ style-element id) "nyxt-stylesheet")
         (ps:chain document head (append-child style-element))
         (ps:chain style-element sheet (insert-rule box-style 0))
         (ps:chain style-element sheet (insert-rule highlighted-style 1)))
       (:catch (error)))))

  (defun create-match-object (body identifier)
    (ps:create "type" "match" "identifier" identifier "body" body))

  (defun create-match-span (body identifier)
    (ps:let* ((el (ps:chain document (create-element "span"))))
      (setf (ps:@ el class-name) "nyxt-hint")
      (setf (ps:@ el text-content) body)
      (setf (ps:@ el id) (+ "nyxt-hint-" identifier))
      el))

  (defun get-substring (string query index)
    "Return the substring and preceding/trailing text for a given
     index."
    (let* ((character-preview-count 40))
           (ps:chain string
                     (substring (- index character-preview-count)
                                (+ index (length query) character-preview-count)))))

  (defun get-substring-indices (query string)
    "Get the indices of all matching substrings."
    (let ((rgx (ps:new (|RegExp| query (if (ps:lisp case-sensitive-p) "" "i"))))
          (index (- (length query))))
      (loop with subindex = 0
            until (= subindex -1)
            do (setf subindex (ps:chain string (search rgx)))
               (setf string (ps:chain string (substring (+ subindex (length query)))))
               (setf index (+ index subindex (length query)))
            when (not (= subindex -1))
              collect index)))

  (defun matches-from-node (node query)
    "Return all of substrings that match the search-string."
    (when (= (ps:chain (typeof (ps:@ node node-value))) "string")
      (let* ((node-text (ps:@ node text-content))
             (substring-indices (get-substring-indices query node-text))
             (node-identifier (incf (ps:chain *nodes* identifier)))
             (new-node (ps:chain document (create-element "span"))))
        (setf (ps:@ new-node class-name) "nyxt-search-node")
        (setf (ps:@ new-node id) node-identifier)
        (setf (aref *nodes* node-identifier) node)
        (when (> (length substring-indices) 0)
          (loop for index in substring-indices
                with last-index = 0
                do (incf *identifier*)
                   (ps:chain new-node (append-child (ps:chain document (create-text-node (ps:chain node-text (substring last-index index))))))
                   (ps:chain new-node (append-child (create-match-span (ps:chain node-text (substring index (+ index (length query)))) *identifier*)))
                   (setf last-index (+ (length query) index))
                   (ps:chain *matches* (push (create-match-object (get-substring node-text query index) *identifier*)))
                finally (progn
                          (ps:chain new-node (append-child (ps:chain document (create-text-node (ps:chain node-text (substring (+ (length query) index)))))))
                          (ps:chain *node-replacements*
                                    (push (list node new-node)))))))))

  (defun replace-original-nodes ()
    "Replace original nodes with recreated search nodes"
    (loop for node-pair in *node-replacements*
          do (ps:chain (elt node-pair 0) (replace-with (elt node-pair 1)))))

  (defun walk-document (node process-node)
    (when (and node (not (ps:chain node first-child)))
      (funcall process-node node (ps:lisp query)))
    (setf node (ps:chain node first-child))
    (loop while node
          do (walk-document node process-node)
          do (setf node (ps:chain node next-sibling))))

  (defun remove-search-nodes ()
    "Removes all the search elements"
    (ps:dolist (node (qsa document ".nyxt-search-node"))
      (ps:chain node (replace-with (aref *nodes* (ps:@ node id))))))

  (let ((*matches* (array))
        (*node-replacements* (array))
        (*identifier* 0))
    (add-stylesheet)
    (remove-search-nodes)
    (setf (ps:chain *nodes* identifier) 0)
    (walk-document (ps:chain document body) matches-from-node)
    (replace-original-nodes)
    (ps:chain |json| (stringify *matches*))))

(define-class search-match ()
  ((identifier)
   (body)
   (buffer))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(defmethod prompter:object-attributes ((match search-match))
  `(("Default" ,(body match))
    ("ID" ,(princ-to-string (identifier match)))
    ("Buffer ID" ,(id (buffer match)))
    ("Buffer title" ,(title (buffer match)))))

(defun matches-from-json (matches-json &optional (buffer (current-buffer)))
  (loop for element in (handler-case (cl-json:decode-json-from-string matches-json)
                         (error () nil))
        collect (make-instance 'search-match
                               :identifier (cdr (assoc :identifier element))
                               :body (cdr (assoc :body element))
                               :buffer buffer)))

(define-command remove-search-hints ()
  "Remove all search hints."
  (pflet ((remove-search-hints ()
            (defun qsa (context selector)
              "Alias of document.querySelectorAll"
              (ps:chain context (query-selector-all selector)))
            (defun remove-search-nodes ()
              "Removes all the search elements"
              (ps:dolist (node (qsa document ".nyxt-search-node"))
                (ps:chain node (replace-with (aref *nodes* (ps:@ node id))))))
            (remove-search-nodes)))
    (remove-search-hints)))

(define-class search-buffer-source (prompter:source)
  ((case-sensitive-p nil)
   (buffer (current-buffer))
   (minimum-search-length 3)
   (prompter:name "Search buffer")
   (prompter:follow-p t)
   (prompter:filter nil)
   (prompter:filter-preprocessor
    (lambda (preprocessed-suggestions source input)
      (declare (ignore preprocessed-suggestions))
      (if (>= (length input) (minimum-search-length source))
          (let ((input (str:replace-all " " " " input))
                (buffer (buffer source)))
            (with-current-buffer buffer
              (matches-from-json
               (query-buffer
                :query input
                :case-sensitive-p (case-sensitive-p source))
               buffer)))
          (progn
            (remove-search-hints)
            '()))))
   (prompter:persistent-action (lambda (suggestion)
                                 ;; TODO: rewrite prompt-buffer-selection-highlight-hint
                                 (set-current-buffer (buffer suggestion) :focus nil)
                                 (prompt-buffer-selection-highlight-hint :scroll t)))
   (prompter:destructor (lambda (prompter source)
                          (declare (ignore prompter source))
                          (remove-focus))))
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(defmethod initialize-instance :after ((source search-buffer-source) &key)
  (setf (prompter:name source)
        (format nil "~a (~a+ characters)"
                (prompter:name source)
                (minimum-search-length source))))

(define-command search-buffer (&key case-sensitive-p)
  "Start a search on the current buffer."
  (prompt
   :prompt "Search text"
   :sources (list
             (make-instance 'search-buffer-source
                            :case-sensitive-p case-sensitive-p))))

(define-command search-buffers (&key case-sensitive-p)
  "Start a search on buffers."
  (let ((buffers (prompt
                  :prompt "Search buffer(s)"
                  :sources (list (make-instance 'buffer-source ; TODO: Define class?
                                                :actions '()
                                                :multi-selection-p t)))))
    (prompt
     :prompt "Search text"
     :sources (mapcar (lambda (buffer)
                        (make-instance 'search-buffer-source
                                       :name (format nil "Search ~a" (if (url-empty-p (url buffer))
                                                                         (title buffer)
                                                                         (url buffer)))
                                       :case-sensitive-p case-sensitive-p
                                       :buffer buffer))
                      buffers))))

(defun recursive-search (depth url-list &key case-sensitive-p)
  "Start a search on URLs that are linked in the current buffer, with arbitrary
DEPTH."
  ;; check if the prompt has returned
  (let* ((urls (alexandria:flatten
                (nyxt::interactive-recursive-links depth url-list)))
         (new-buffers)
         (buffers (mapcar
                   (lambda (url) (let ((buffer-exists-p (first (buffer-list :url url))))
                              (if buffer-exists-p
                                  buffer-exists-p
                                  (let
                                      ((new-buffer
                                         (make-buffer
                                          :modes (append
                                                  '(noimage-mode)
                                                  (active-modes))
                                          :url url)))
                                    (push new-buffer new-buffers)
                                    new-buffer))))
                   urls)))
    (mapcar #'nyxt::buffer-delete
            (set-difference
             new-buffers
             (list (slot-value
                    (first (prompt
                            :prompt "Search text"
                            :sources
                            (mapcar (lambda (buffer)
                                      (make-instance
                                       'search-buffer-source
                                       :name (format nil "Search ~a"
                                                     (if (url-empty-p (url buffer))
                                                         (title buffer)
                                                         (url buffer)))
                                       :case-sensitive-p case-sensitive-p
                                       :buffer buffer))
                                    buffers)))
                    'buffer))))))

(define-command recursive-search-from-current-buffer (&key case-sensitive-p)
  "Start a search on URLs that are linked in the current buffer, with arbitrary
DEPTH."
  ;; TODO how can the an arbitrary depth be returned from the prompter as well
  ;; as providing suggestions.
  (recursive-search (parse-integer
                     (first
                      (prompt
                       :prompt "Choose Depth"
                       :sources (list (make-instance 'prompter:raw-source
                                                     :name "Depth")
                                      (make-instance 'prompter:source
												                             :name "Depth"
												                             :constructor '("1" "2")))
                       :hide-suggestion-count-p t))
                     :junk-allowed t)
                    (list (url (current-buffer)))
                    :case-sensitive-p case-sensitive-p))
