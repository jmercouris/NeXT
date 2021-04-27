;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

(hooks:define-hook-type keymaps-buffer (function (list-of-keymaps buffer)
                                                 (values &optional list-of-keymaps buffer)))
(export-always '(make-hook-keymaps-buffer make-handler-keymaps-buffer))
(hooks:define-hook-type uri->uri (function (quri:uri) quri:uri))

(define-class buffer ()
  ((id ""
       :documentation "Unique identifier for a buffer.
Dead buffers or placeholder buffers (i.e. those not associated with a web view)
have an empty ID.")
   (data-profile (make-instance (or (find-data-profile (getf *options* :data-profile))
                                    'default-data-profile))
                 :type data-profile
                 :documentation "Profile to use for all persisted files.
See the `data-path' class and the `expand-path' function.")
   ;; TODO: Or maybe a dead-buffer should just be a buffer history?
   (url (quri:uri ""))
   (url-at-point (quri:uri ""))
   (title "")
   (last-access (local-time:now)
                :export nil
                :documentation "Timestamp when the buffer was last switched to.")
   (modes :initform '()
          :documentation "The list of mode instances.
Modes are instantiated after the `default-modes' slot, with `initialize-modes'
and not in the initform so that the instantiation form can access the
initialized buffer.")
   (default-modes '(web-mode base-mode)
                  :type list-of-symbols
                  :documentation "The symbols of the modes to instantiate on buffer creation.
The mode instances are stored in the `modes' slot.")
   (enable-mode-hook (make-hook-mode)
                     :type hook-mode
                     :documentation "Hook run on every mode activation,
after the mode-specific hook.")
   (disable-mode-hook (make-hook-mode)
                      :type hook-mode
                      :documentation "Hook run on every mode deactivation,
after the mode-specific hook.")
   (keymap-scheme-name scheme:cua
                       :documentation "The keymap scheme that will be used for all modes in the current buffer.")
   (search-engines (list (make-instance 'search-engine
                                        :shortcut "wiki"
                                        :search-url "https://en.wikipedia.org/w/index.php?search=~a"
                                        :fallback-url "https://en.wikipedia.org/")
                         (make-instance 'search-engine
                                        :shortcut "ddg"
                                        :search-url "https://duckduckgo.com/?q=~a"
                                        :fallback-url "https://duckduckgo.com/"))
                   :type (cons search-engine *)
                   :documentation "A list of the `search-engine' objects.
You can invoke them from the prompt-buffer by prefixing your query with SHORTCUT.
If the query is empty, FALLBACK-URL is loaded instead.  If
FALLBACK-URL is empty, SEARCH-URL is used on an empty search.

The default search engine (as pre `default-search-engine') is used when the
query is not a valid URL, or the first keyword is not recognized.")
   (current-keymaps-hook
    (make-hook-keymaps-buffer
     :combination #'hooks:combine-composed-hook)
    :type hook-keymaps-buffer
    :documentation "Hook run as a return value of `current-keymaps'.")
   (conservative-word-move
    nil
    :documentation "If non-nil, the cursor moves to the end
(resp. beginning) of the word when `move-forward-word'
(resp. `move-backward-word') is called.")
   (override-map (let ((map (make-keymap "override-map")))
                   (define-key map
                     "C-space" 'execute-command))
                 :documentation "Keymap that overrides all other bindings.
No libraries should ever touch the override-map, this is left for the user to
customize to their needs.

Example:

\(define-configuration buffer
  ((override-map (let ((map (make-keymap \"override-map\")))
                             (define-key map
                               \"M-x\" 'execute-command
                               \"C-q\" 'quit)
                   map))))")
   (forward-input-events-p t
                           :documentation "When non-nil, keyboard events are
forwarded to the renderer when no binding is found.  Pointer
events (e.g. mouse events) are not affected by this, they are always
forwarded when no binding is found.")
   (last-event nil
               :type t
               :export nil
               ;; TODO: Store multiple events?  Maybe when implementing keyboard macros.
               :documentation "The last event received in the current buffer.")
   (pre-request-hook (make-hook-resource
                      :combination #'combine-composed-hook-until-nil)
                     :type hook-resource
                     :documentation "Hook run before the `request-resource-hook'.
One example of its application is `auto-mode' that changes mode setup. Any
action on modes that can possibly change the handlers in `request-resource-hook'
should find its place there.")
   (request-resource-scheme (define-scheme "request-resource"
                              scheme:cua
                              (list
                               "C-button1" 'request-resource-open-url-focus
                               "button2" 'request-resource-open-url-focus
                               "C-shift-button1" 'request-resource-open-url))
                            :documentation "This keymap can be looked up when
`request-resource-hook' handlers run.
The functions are expected to take key arguments like `:url'.")
   (request-resource-hook (make-hook-resource
                           :combination #'combine-composed-hook-until-nil
                           :handlers (list #'request-resource))
                          :type hook-resource
                          :documentation "Hook run on every resource load.
The handlers are composed, passing a `request-data'
until one of them returns nil or all handlers apply successfully.

Newest hook is run first.
If a `request-data' object is returned, it gets passed to other handlers
or right to the renderer if there are no more handlers.
If nil is returned, stop the hook and cancel the resource load.

The current buffer URL should not be relied upon.  With WebKitGTK, it is the same
as (url REQUEST-DATA).
If you need to access the URL before this request, inspect the web-mode history.

There's no more ability to pass the results to the renderer with :FORWARD.

Example:

\(define-configuration buffer
  ((request-resource-hook
    (reduce #'hooks:add-hook
            (mapcar #'make-handler-resource (list #'old-reddit-handler #'auto-proxy-handler))
            :initial-value %slot-default%))))")
   (default-new-buffer-url (quri:uri "https://nyxt.atlas.engineer/start")
                           :documentation "The URL set to a new blank buffer opened by Nyxt.")
   (scroll-distance 50
                    :documentation "The distance scroll-down or scroll-up will scroll.")
   (smooth-scrolling nil
                     :documentation "Whether to scroll smoothly with the mouse.")
   (horizontal-scroll-distance 50
                               :documentation "Horizontal scroll distance. The
distance scroll-left or scroll-right will scroll.")
   (current-zoom-ratio 1.0
                       :documentation "The current zoom relative to the default zoom.")
   (zoom-ratio-step 0.2
                    :documentation "The step size for zooming in and out.")
   (zoom-ratio-min 0.2
                   :documentation "The minimum zoom ratio relative to the default.")
   (zoom-ratio-max 5.0
                   :documentation "The maximum zoom ratio relative to the default.")
   (zoom-ratio-default 1.0
                       :documentation "The default zoom ratio.")
   (page-scroll-ratio 0.90
                      :documentation "The ratio of the page to scroll.
A value of 0.95 means that the bottom 5% will be the top 5% when scrolling
down.")
   (buffer-load-hook (make-hook-uri->uri
                      :combination #'hooks:combine-composed-hook)
                     :type hook-uri->uri
                     :accessor nil
                     :export nil ; TODO: Export?  Maybe not since `request-resource-hook' mostly supersedes it.
                     :documentation "Hook run in `buffer-load' after `parse-url' was processed.
The handlers take the URL going to be loaded as argument
and must return a (possibly new) URL.")
   (buffer-loaded-hook (make-hook-buffer)
                       :type hook-buffer
                       :documentation "Hook run when `load-status' is set to
`:finished'.
The handlers take the buffer as argument.")
   (buffer-delete-hook (make-hook-buffer)
                       :type hook-buffer
                       :documentation "Hook run before `buffer-delete' takes effect.
The handlers take the buffer as argument.")
   (password-interface (make-password-interface)
                       :type password::password-interface
                       :documentation "The current password interface.
See `password:*interfaces*' for the list of all currently registered interfaces.
To use, say, KeepassXC, set this slot to

  (make-instance 'password:user-keepassxc-interface)

Password interfaces may have user classes (that is, prefixed with 'user-' as in
the above example), in which case you can use `define-configuration' on them.")
   (download-path (make-instance 'download-data-path
                                 :dirname (xdg-download-dir))
                  :type data-path
                  :documentation "Path of directory where downloads will be
stored.  Nil means use system default.
Downloads are kept in browser's `user-data', keyed by the expanded `download-path'.")
   (download-engine :initform :renderer
                    :type symbol
                    :documentation "Select a download engine to use,
such as :lisp or :renderer.")
   (history-path (make-instance 'history-data-path :basename "default"
                                                   :dirname (uiop:xdg-data-home +data-root+ "history"))
                 :type data-path
                 :documentation "
The path where the system will create/save the global history.
History data is kept in browser's `user-data', keyed by the expanded `history-path'.")
   (bookmarks-path (make-instance 'bookmarks-data-path :basename "bookmarks")
                   :type data-path
                   :documentation "
The path where the system will create/save the bookmarks.
Bookmarks' data is kept in browser's `user-data', keyed by the expanded `bookmarks-path'.")
   (auto-mode-rules-path (make-instance 'auto-mode-rules-data-path
                                        :basename "auto-mode-rules")
                         :type data-path
                         :documentation "The path where the auto-mode rules are saved.
Rules are kept in browser's `user-data', keyed by the expanded `auto-mode-rules-path'.")
   (standard-output-path (make-instance 'standard-output-data-path :basename "standard-out.txt")
                         :type data-path
                         :documentation "Path where `*standard-output*' can be written to.")
   (error-output-path (make-instance 'error-output-data-path :basename "standard-error.txt")
                      :type data-path
                      :documentation "Path where `*error-output*' can be written to."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "A buffer is the fundamental unit of displayed content.
Buffers result from the computations of a web renderer, which generates a visual
representation of HTML documents.

Rendered URLs or the Nyxt's manual qualify as examples.  Buffers are fully
separated from one another, so that each has its own behaviour and settings."))

(define-user-class buffer)

(define-class web-buffer (user-buffer)
  ((default-modes '(certificate-exception-mode web-mode base-mode)
                  :type list-of-symbols
                  :documentation "The symbols of the modes to instantiate on buffer creation.
The mode instances are stored in the `modes' slot.")
   (load-status :unloaded ; TODO: Rename to `status' to be consistent with download-mode?
                :type (member :loading
                              :finished
                              :unloaded
                              :failed)
                :accessor nil
                :reader load-status
                :export nil ; TODO: Need to decide if we want progress / errors before exposing to the user.
                :documentation "The status of the buffer.
- `:loading' when loading a web resource.
- `:finished' when done loading a web resource.
- `:unloaded' for buffers that have not been loaded yet, like
  session-restored buffers, dead buffers or new buffers that haven't started the
  loading process yet.")
   (proxy nil
          :accessor nil
          :type (or proxy null)
          :documentation "Proxy for buffer.")
   (certificate-exceptions '()
                           :type list-of-strings
                           :documentation "A list of hostnames for which certificate errors shall be ignored.")
   (cookies-path (make-instance 'cookies-data-path :basename "cookies.txt")
                 :type data-path
                 :documentation "The path where cookies are stored.  Not all
renderers might support this.")
   (default-cookie-policy :no-third-party
                          :type cookie-policy
                          :documentation "Cookie policy of new buffers.
Must be one of `:always' (accept all cookies), `:never' (reject all cookies),
`:no-third-party' (accept cookies for current website only)."))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(define-user-class web-buffer)

(defmethod initialize-instance :after ((buffer web-buffer) &key)
  (when (expand-path (cookies-path buffer))
    (ensure-parent-exists (expand-path (cookies-path buffer)))))

(defmethod (setf load-status) (status (buffer web-buffer))
  (setf (slot-value buffer 'load-status) status)
  (when (eq status :finished)
    (bt:make-thread
     (lambda () (hooks:run-hook (buffer-loaded-hook buffer) buffer)))))

(define-class nosave-buffer (user-web-buffer)
  ((data-profile (make-instance 'nosave-data-profile))
   (default-modes '(reduce-tracking-mode certificate-exception-mode
                    web-mode base-mode)))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(define-user-class nosave-buffer)

(define-class internal-buffer (user-buffer)
  ((style #.(cl-css:css
             '((body
                :line-height "24px"
                :margin-left "20px"
                :margin-top "20px")
               (h1
                :font-family "Helvetica Neue, Helvetica"
                :font-weight 500)
               (h2
                :font-family "Helvetica Neue, Helvetica"
                :font-weight 500)
               (h3
                :font-family "Helvetica Neue, Helvetica"
                :font-weight 500)
               (h4
                :font-family "Helvetica Neue, Helvetica"
                :font-weight 500)
               (h5
                :font-family "Helvetica Neue, Helvetica"
                :font-weight 500)
               (h6
                :font-family "Helvetica Neue, Helvetica"
                :font-weight 500)
               (hr
                :height "3px"
                :border-radius "2px"
                :border-width "0"
                :color "lightgray"
                :background-color "lightgray")
               (.button
                :display "inline-block"
                :background-color "darkgray"
                :color "white"
                :text-decoration "none"
                :border-radius "2px"
                :padding "6px"
                :margin-left "2px"
                :margin-right "2px")
               (|.button:hover|
                :color "black")
               (|.button:visited|
                :color "white")
               (|.button:active|
                :color "white")))))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(define-user-class internal-buffer)

(define-class editor-buffer (internal-buffer)
  ((file :documentation "The file being edited.")
   (default-modes '(plaintext-editor-mode base-mode))
   (url (quri:uri "editor-buffer"))
   (title "editor-buffer"))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name))
  (:documentation "Each editor buffer matches a file. Each editor buffer
  contains an editor mode instance."))

(define-user-class editor-buffer)

(defun make-dummy-buffer ()
  ;; Internal buffers are lighter than full-blown buffers which can have a
  ;; WebKit context, etc.
  (make-instance 'user-internal-buffer))

(define-class status-buffer (user-internal-buffer)
  ((height 20
           :type integer
           :documentation "The height of the status buffer in pixels.")
   (glyph-mode-presentation-p
    nil
    :documentation "Display the modes as a list of glyphs.")
   (style #.(cl-css:css
             '((body
                :background "rgb(200, 200, 200)"
                :font-size "14px"
                :color "rgb(32, 32, 32)"
                :padding 0
                :margin 0
                :line-height "20px")
               (".arrow"
                :width "10px"
                :height "20px")
               (".arrow-right"
                :clip-path "polygon(0 100%, 100% 50%, 0 0)")
               (".arrow-left"
                :clip-path "polygon(0 50%, 100% 100%, 100% 0)")
               ("#container"
                :display "grid"
                ;; Columns: controls, arrow, url, arrow, tabs, arrow, modes
                :grid-template-columns "115px 10px 1fr 10px 2fr 10px 250px"
                :overflow-y "hidden")
               ("#controls"
                :background-color "rgb(80,80,80)"
                :padding-left "5px"
                :overflow "hidden"
                :white-space "nowrap")
               ("#url"
                :background-color "rgb(120,120,120)"
                :min-width "100px"
                :text-overflow "ellipsis"
                :overflow-x "hidden"
                :white-space "nowrap"
                :padding-left "15px"
                :padding-right "10px"
                :margin-left "-10px")
               ("#tabs"
                :min-width "100px"
                :background-color "rgb(160,160,160)"
                :padding-left "15px"
                :margin-left "-10px"
                :margin-right "-10px"
                :overflow-x "scroll"
                :text-align "left")
               ("#tabs::-webkit-scrollbar"
                :display "none")
               (.tab
                :color "rgb(250, 250, 250)"
                :white-space "nowrap"
                :text-decoration "none"
                :padding-left "5px"
                :padding-right "5px")
               (".tab:hover"
                :color "black")
               ("#modes"
                :background-color "rgb(120,120,120)"
                :color "rgb(230, 230, 230)"
                :text-align "right"
                :padding-right "5px"
                :overflow-x "scroll"
                :white-space "nowrap")
               ("#modes::-webkit-scrollbar"
                :display "none")
               (.button
                :color "rgb(250, 250, 250)"
                :text-decoration "none"
                :padding-left "2px"
                :padding-right "2px"
                :margin-left "2px"
                :margin-right "2px")
               (|.button:hover|
                :color "black")))))
  (:export-class-name-p t)
  (:export-accessor-names-p t)
  (:export-predicate-name-p t)
  (:accessor-name-transformer (hu.dwim.defclass-star:make-name-transformer name)))

(define-user-class status-buffer)

(defmethod proxy ((buffer buffer))
  (slot-value buffer 'proxy))

(defmethod (setf proxy) (proxy (buffer buffer))
  (setf (slot-value buffer 'proxy) proxy)
  (if proxy
      (ffi-buffer-set-proxy buffer
                            (server-address proxy)
                            (allowlist proxy))
      (ffi-buffer-set-proxy buffer
                            (quri:uri "")
                            nil)))

(declaim (ftype (function (buffer &key (:downloads-only boolean))) proxy-adress))
(defun proxy-address (buffer &key (downloads-only nil))
  "Return the proxy address, nil if not set.
If DOWNLOADS-ONLY is non-nil, then it only returns the proxy address (if any)
when `proxied-downloads-p' is true."
  (let* ((proxy (and buffer (proxy buffer)))
         (proxied-downloads (and proxy (proxied-downloads-p proxy))))
    (when (or (not downloads-only)
              proxied-downloads)
      (server-address proxy))))

(defmethod initialize-modes ((buffer buffer))
  "Initialize BUFFER modes.
This must be called after BUFFER has been created by the renderer.
See `buffer-make'."
  (dolist (mode-symbol (reverse (default-modes buffer)))
    (make-mode mode-symbol buffer)))

(defun load-failed-p (buffer)
  "Only web-buffer loads can fail."
  (and (web-buffer-p buffer)
       (eq (load-status buffer) :failed)))

(export-always 'on-signal-notify-uri)
(defmethod on-signal-notify-uri ((buffer buffer) no-uri)
  "Set BUFFER's `url' slot, then dispatch `on-signal-notify-uri' over the
BUFFER's modes."
  (declare (ignore no-uri))
  (let ((view-url (ffi-buffer-uri buffer)))
    (when (or (not (load-failed-p buffer))
              (not (url-empty-p view-url)))
      ;; When a buffer fails to load and `ffi-buffer-uri' returns an empty
      ;; URL, we don't set (url buffer) to keep access to the old value.
      (setf (url buffer) (ffi-buffer-uri buffer))))
  (dolist (mode (modes buffer))
    (on-signal-notify-uri mode (url buffer)))
  (url buffer))

(defmethod on-signal-notify-uri ((buffer internal-buffer) no-uri)
  "Internal buffers don't load external resources and as such don't need URI
change notifications.
In particular, we don't want to register a URL in the history via the `web-mode'
notification."
  ;; TODO: We should not ignore this notification since it may be used by modes.
  ;; In particular, we receive a legit notify::uri when clicking on an anchor URL.
  (declare (ignore no-uri))
  (url buffer))

(export-always 'on-signal-notify-title)
(defmethod on-signal-notify-title ((buffer buffer) no-title)
  "Set BUFFER's `title' slot, then dispatch `on-signal-notify-title' over the
BUFFER's modes."
  (declare (ignore no-title))
  (setf (title buffer) (ffi-buffer-title buffer))
  (dolist (mode (modes buffer))
    (on-signal-notify-title mode (url buffer)))
  (title buffer))

(export-always 'on-signal-load-committed)
(defmethod on-signal-load-committed ((buffer buffer) url)
  (dolist (mode (modes buffer))
    (on-signal-load-committed mode url)))

(export-always 'on-signal-load-finished)
(defmethod on-signal-load-finished ((buffer buffer) url)
  (dolist (mode (modes buffer))
    (on-signal-load-finished mode url)))

(defun default-mode-symbols ()
  "Return default mode symbols (with package prefix)."
  (let ((default-modes
          (second (getf (mopu:slot-properties (find-class 'buffer) 'default-modes)
                        :initform))))
    (mapcar (alex:compose #'name #'mode-command) default-modes)))

(hooks:define-hook-type buffer (function (buffer)))

(defmethod prompter:object-attributes ((buffer buffer))
  `(("URL" ,(render-url (url buffer)))
    ("Title" ,(title buffer))))

(define-command make-buffer (&key (title "") modes (url "") parent-buffer (load-url-p t))
  "Create a new buffer.
MODES is a list of mode symbols.
If URL is `:default', use `default-new-buffer-url'.
PARENT-BUFFER is useful when we want to record buffer- and history relationships.
LOAD-URL-P controls whether to load URL right at buffer creation."
  (let* ((buffer (buffer-make *browser*
                              :title title
                              :default-modes modes
                              :parent-buffer parent-buffer))
         (url (if (eq url :default)
                  (default-new-buffer-url buffer)
                  url)))
    (unless (url-empty-p url)
      (if load-url-p
          (buffer-load url :buffer buffer)
          (setf (url buffer) (quri:uri url))))
    buffer))

(define-command make-nosave-buffer (&key (title "") modes (url "") (load-url-p t))
  "Create a new buffer that won't save anything to the filesystem.
MODES is a list of mode symbols.
If URL is `:default', use `default-new-buffer-url'.
LOAD-URL-P controls whether to load URL right at buffer creation."
  (let* ((buffer (buffer-make *browser* :title title
                                        :default-modes modes
                                        :buffer-class 'user-nosave-buffer))
         (url (if (eq url :default)
                  (default-new-buffer-url buffer)
                  url)))
    (unless (url-empty-p url)
      (if load-url-p
          (buffer-load url :buffer buffer)
          (setf (url buffer) (quri:uri url))))
    buffer))

(define-command make-internal-buffer (&key (title "") modes no-history-p)
  "Create a new buffer.
MODES is a list of mode symbols.
If URL is `:default', use `default-new-buffer-url'."
  (buffer-make *browser* :title title
                         :default-modes modes
                         :buffer-class 'user-internal-buffer
                         :no-history-p no-history-p))

(define-command make-editor-buffer (&key (title "") modes)
  "Create a new editor buffer."
  (buffer-make *browser* :title title
                         :default-modes modes
                         :buffer-class 'user-editor-buffer))

(declaim (ftype (function (browser &key (:title string)
                                   (:data-profile data-profile)
                                   (:default-modes list)
                                   (:dead-buffer buffer)
                                   (:nosave-buffer-p boolean)
                                   (:buffer-class symbol)
                                   (:parent-buffer buffer)
                                   (:no-history-p boolean)))
                buffer-make))
(defun buffer-make (browser &key data-profile title default-modes
                                 dead-buffer (buffer-class 'user-web-buffer)
                                 parent-buffer no-history-p
                                 (nosave-buffer-p (nosave-buffer-p parent-buffer)))
  "Make buffer with title TITLE and modes DEFAULT-MODES.
Run `*browser*'s `buffer-make-hook' over the created buffer before returning it.
If DEAD-BUFFER is a dead buffer, recreate its web view and give it a new ID."
  (let ((buffer (if dead-buffer
                    (progn
                      ;; Dead buffer ID must be renewed before calling `ffi-buffer-make'.
                      (setf (id dead-buffer) (get-unique-buffer-identifier *browser*))
                      (ffi-buffer-make dead-buffer))
                    (apply #'make-instance buffer-class
                           :id (get-unique-buffer-identifier *browser*)
                           (append (when title `(:title ,title))
                                   (when default-modes `(:default-modes ,default-modes))
                                   (when data-profile `(:data-profile ,data-profile)))))))
    (hooks:run-hook (buffer-before-make-hook *browser*) buffer)
    ;; Modes might require that buffer exists, so we need to initialize them
    ;; after the view has been created.
    (initialize-modes buffer)
    (when dead-buffer                   ; TODO: URL should be already set.  Useless?
      (setf (url buffer) (url dead-buffer)))
    (buffers-set (id buffer) buffer)
    (unless no-history-p
      ;; When we create buffer, current one can override the
      ;; data-profile of the created buffer. This is dangerous,
      ;; especially for nosave buffers.
      (with-current-buffer buffer
        ;; Register buffer in global history:
        (with-data-access (history (history-path buffer)
                                   :default (make-history-tree buffer))
          ;; Owner may already exist if history was just created with the above
          ;; default value.
          (unless (htree:owner history (id buffer))
            (htree:add-owner history (id buffer)
                             :creator-id (when (and parent-buffer
                                                    (not nosave-buffer-p)
                                                    (not (nosave-buffer-p parent-buffer)))
                                           (id parent-buffer)))))))
    ;; Run hooks before `initialize-modes' to allow for last-minute modification
    ;; of the default modes.
    (hooks:run-hook (buffer-make-hook browser) buffer)
    buffer))

(declaim (ftype (function (buffer)) add-to-recent-buffers))
(defun add-to-recent-buffers (buffer)
  "Create a recent-buffer from given buffer and add it to `recent-buffers'."
  ;; Make sure it's a dead buffer:
  (setf (id buffer) "")
  (containers:delete-item-if (recent-buffers *browser*) (buffer-match-predicate buffer))
  (containers:insert-item (recent-buffers *browser*) buffer))

(declaim (ftype (function (buffer)) buffer-delete))
(defun buffer-delete (buffer)
  (hooks:run-hook (buffer-delete-hook buffer) buffer)
  (let ((parent-window (find buffer (window-list) :key 'active-buffer)))
    (when parent-window
      (let ((replacement-buffer (or (first (get-inactive-buffers))
                                    (make-buffer :url :default))))
        (window-set-buffer parent-window replacement-buffer)))
    (ffi-buffer-delete buffer)
    (buffers-delete (id buffer))
    ;; (setf (id buffer) "") ; TODO: Reset ID?
    (with-data-access (history (history-path buffer))
      (htree:delete-owner history (id buffer)))
    (add-to-recent-buffers buffer)
    (store (data-profile buffer) (history-path buffer))))

(export-always 'buffer-list)
(defun buffer-list (&key sort-by-time domain url)
  (let* ((buffer-list (alex:hash-table-values (buffers *browser*)))
         (buffer-list
           (if sort-by-time
               (sort buffer-list #'local-time:timestamp> :key #'last-access)
               buffer-list))
         (buffer-list
           (if domain
               (remove-if-not
                (lambda (i) (equal domain (quri:uri-domain (url i)))) buffer-list)
               buffer-list))
         (buffer-list
           (if url
               (remove-if-not (lambda (i) (quri:uri= url (url i))) buffer-list)
               buffer-list)))
    buffer-list))

(defun buffers-get (id)
  (gethash id (slot-value *browser* 'buffers)))

(defun buffers-set (id buffer)
  (setf (gethash id (slot-value *browser* 'buffers)) buffer)
  (print-status))

(defun buffers-delete (id)
  (remhash id (slot-value *browser* 'buffers))
  (print-status))

(export-always 'window-list)
(defun window-list ()
  (alex:hash-table-values (windows *browser*)))

(export-always 'window-set-buffer)
(defun window-set-buffer (window buffer &key (focus t))
  "Set BROWSER's WINDOW buffer to BUFFER.
Run WINDOW's `window-set-buffer-hook' over WINDOW and BUFFER before
proceeding."
  (hooks:run-hook (window-set-buffer-hook window) window buffer)
  ;; The current buffer last-access time is set to now to ensure it becomes the
  ;; second newest buffer.  If we didn't update the access time, the buffer
  ;; last-access time could be older than, say, buffers opened in the
  ;; background.
  (setf (last-access (active-buffer window)) (local-time:now))
  (let ((window-with-same-buffer (find buffer (delete window (window-list))
                                       :key #'active-buffer)))
    ;; When switching buffers, `current-buffer' is still the old one,
    ;; so path is expanded/queried by the rules of the old
    ;; buffer. That's not desirable, especially for nosave-buffers.
    (with-current-buffer buffer
      (unless (internal-buffer-p buffer)
        (with-data-access (history (history-path buffer)
                           :default (make-history-tree buffer))
          (htree:set-current-owner history (id buffer)))))
    (if window-with-same-buffer ;; if visible on screen perform swap, otherwise just show
        (let ((temp-buffer (make-dummy-buffer))
              (old-buffer (active-buffer window)))
          (log:debug "Swapping old buffer ~a with other window ~a to switch to ~a"
                     (render-url (url old-buffer))
                     (render-url (url (active-buffer window-with-same-buffer)))
                     (render-url (url buffer)))
          (ffi-window-set-buffer window-with-same-buffer temp-buffer)
          (ffi-window-set-buffer window buffer :focus focus)
          (setf (active-buffer window) buffer)
          (window-set-buffer window-with-same-buffer old-buffer)
          (ffi-buffer-delete temp-buffer))
        (progn
          (ffi-window-set-buffer window buffer :focus focus)
          (setf (active-buffer window) buffer)))
    (setf (last-access buffer) (local-time:now))
    ;; So that `current-buffer' returns the new value if buffer was
    ;; switched inside a `with-current-buffer':
    (setf %buffer nil)
    (set-window-title window buffer)
    (print-status nil window)
    (when (and (web-buffer-p buffer)
               (eq (load-status buffer) :unloaded))
      (reload-buffer buffer))))

(defun last-active-buffer ()
  "Return buffer with most recent `last-access'."
  (first (sort (buffer-list)
               #'local-time:timestamp> :key #'last-access)))

(defun get-inactive-buffers ()
  "Return inactive buffers sorted by last-access timestamp, or NIL if none."
  (let ((active-buffers
          (mapcar #'active-buffer (window-list)))
        (buffers (buffer-list)))
    (alex:when-let ((diff (set-difference buffers active-buffers)))
       ;; Display the most recent inactive buffer.
       (sort diff #'local-time:timestamp> :key #'last-access))))

(define-command copy-url ()
  "Save current URL to clipboard."
  (copy-to-clipboard (render-url (url (current-buffer))))
  (echo "~a copied to clipboard." (render-url (url (current-buffer)))))

(define-command copy-title ()
  "Save current page title to clipboard."
  (copy-to-clipboard (title (current-buffer)))
  (echo "~a copied to clipboard." (title (current-buffer))))

(defun buffer-initial-suggestions (&key current-is-last-p domain)
  (let ((buffers (buffer-list :sort-by-time t :domain domain)))
    (when current-is-last-p
      (setf buffers (alex:rotate buffers -1)))
    buffers))

(define-class buffer-source (prompter:source)
  ((prompter:name "Buffer list")
   (prompter:constructor (buffer-initial-suggestions :current-is-last-p nil))
   (prompter:actions (list (make-unmapped-command set-current-buffer)))
   (prompter:follow-p t)
   (prompter:follow-delay 0.1)
   (prompter:persistent-action #'(lambda (buffer)
                                   (set-current-buffer buffer :focus nil)))
   (prompter:destructor (let ((buffer (current-buffer)))
                           (lambda (prompter source)
                             (declare (ignore source))
                             (unless (or (prompter:returned-p prompter)
                                         (eq buffer (current-buffer)))
                               (set-current-buffer buffer))))))
  (:export-class-name-p t))

(define-command switch-buffer (&key id)
  "Switch the active buffer in the current window."
  (if id
      (set-current-buffer (buffers-get id))
      (prompt
       :prompt "Switch to buffer"
       :sources (list (make-instance 'buffer-source
                                     ;; For commodity, the current buffer shouldn't be the first one on the list.
                                     :constructor (buffer-initial-suggestions :current-is-last-p t))))))

(define-command switch-buffer-domain (&key domain (buffer (current-buffer)))
  "Switch the active buffer in the current window from the current domain."
  (let ((domain (or domain (quri:uri-domain (url buffer)))))
    (prompt
     :prompt "Switch to buffer in current domain:"
     :sources (make-instance 'buffer-source
                             :constructor (buffer-list :sort-by-time t
                                                       :domain domain)))))

(defun switch-buffer-or-query-domain (domain)
  "Switch to a buffer if it exists for a given domain, otherwise query
  the user."
  (let* ((matching-buffers (buffer-list :domain domain)))
    (if (eql 1 (length matching-buffers))
        (set-current-buffer (first matching-buffers))
        (switch-buffer-domain :domain domain))))

(define-command make-buffer-focus (&key (url :default) parent-buffer nosave-buffer-p)
  "Switch to a new buffer.
See `make-buffer'."
  (let ((buffer (if nosave-buffer-p
                    (make-nosave-buffer :url url)
                    (make-buffer :url url :parent-buffer parent-buffer))))
    (set-current-buffer buffer)
    buffer))

(define-command delete-buffer (&key id)
  "Query the buffer(s) to delete."
  (if id
      (buffer-delete (gethash id (slot-value *browser* 'buffers)))
      (prompt
       :prompt "Delete buffer(s)"
       :sources (make-instance 'buffer-source
                               :multi-selection-p t
                               :actions (list (make-mapped-command buffer-delete))))))

(define-command reduce-to-buffer (&key (delete t))
  "Query the buffer(s) to \"reduce \" by copying their titles/URLs to a
single buffer, optionally delete them. This function is useful for archiving a
set of useful URLs or preparing a list to send to a someone else."
  (let ((buffers (prompt
                  :prompt "Reduce buffer(s)"
                  :sources (make-instance 'buffer-source
                                          :actions '()
                                          :multi-selection-p t))))
    (with-current-html-buffer (reduced-buffer "*Reduced Buffers*" 'base-mode)
      (markup:markup
       (:style (style reduced-buffer))
       (:h1 "Reduced Buffers:")
       (:div
        (loop for buffer in buffers
              collect
                 (with-current-buffer buffer
                   (markup:markup
                    (:div
                     (:p (:b "Title: ") (title buffer))
                     (:p (:b "URL: ") (:a :href (render-url (url buffer))
                                          (render-url (url buffer))))
                     (:p (:b "Automatically generated summary: ")
                         (:ul
                          (loop for summary-bullet in (analysis:summarize-text
                                                       (document-get-paragraph-contents :limit 10000))
                                collect (markup:markup (:li (str:collapse-whitespaces summary-bullet))))))
                     (:hr ""))))))))
    (when delete (mapcar #'buffer-delete buffers))))

(define-command delete-all-buffers (&key confirmation-p)
  "Delete all buffers, with confirmation."
  (let ((count (length (buffer-list))))
    (if confirmation-p
        (if-confirm ("Are you sure to delete ~a buffer~p?" count count)
                    (mapcar #'buffer-delete (buffer-list)))
        (mapcar #'buffer-delete (buffer-list)))))

(define-command delete-current-buffer (&optional (buffer (current-buffer)))
  "Delete the current buffer, and make the next buffer the current one. If no
other buffers exist, set the url of the current buffer to the start page."
  (buffer-delete buffer))

(define-command delete-other-buffers (&optional (buffer (current-buffer)))
  "Delete all buffers except BUFFER.
When BUFFER is omitted, it defaults to the current one."
  (let* ((all-buffers (buffer-list))
         (buffers-to-delete (remove buffer all-buffers))
         (count (list-length buffers-to-delete)))
    (if-confirm ("Are you sure to delete ~a buffer~p?" count count)
      (mapcar #'buffer-delete buffers-to-delete))))

(export-always 'buffer-load)
(declaim (ftype (function ((or quri:uri string history-entry) &key (:buffer buffer)) t)
                buffer-load))
(defun buffer-load (input-url &key (buffer (current-buffer)))
  "Load INPUT-URL in BUFFER.
If INPUT-URL is a string, it's transformed to a `quri:uri' by `parse-url'.
URL is then transformed by BUFFER's `buffer-load-hook'."
  (let* ((url (typecase input-url
                (string
                 (parse-url input-url))
                (history-entry
                 (url input-url))
                (t
                 input-url)))
         (new-url
           (handler-case
               (hooks:run-hook (slot-value buffer 'buffer-load-hook) url)
             (error (c)
               (log:error "In `buffer-load-hook': ~a" c)
               nil))))
    (when new-url
      (check-type new-url quri:uri)
      (setf url new-url)
      ;; TODO: This condition can be a source of inefficiency.  Besides, it
      ;; partly duplicates the code in `request-resource'.  Can we factor this
      ;; out?
      (cond
        ((and (internal-buffer-p buffer) (equal "lisp" (quri:uri-scheme url)))
         (let ((code (quri:url-decode (schemeless-url url) :lenient t)))
           (log:debug "Evaluate Lisp code from internal buffer: ~a" code)
           (evaluate-async code)))
        ((internal-buffer-p buffer)
         ;; REVIEW: `request-resource' renders the URL, should we do it here as well?
         (make-buffer-focus :url url))
        ((equal "javascript" (quri:uri-scheme url))
         (ffi-buffer-evaluate-javascript buffer (quri:url-decode (quri:uri-path url))))
        (t (ffi-buffer-load buffer url))))))

(defun new-buffer-load (url)
  "Load a URL in a new buffer."
  (make-buffer-focus :url url))

(defun new-nosave-buffer-load (url)
  "Load a URL in a new nosave buffer."
  (make-buffer-focus :url url :nosave-buffer-p t))

(defun new-buffer-load-from-history (history-suggestion)
  "Load a URL in a new buffer (from history)."
  (make-buffer-focus :url (url history-suggestion)))

(defun new-nosave-buffer-load-from-history (history-suggestion)
  "Load a URL in a new nosave buffer (from history)."
  (make-buffer-focus :url (url history-suggestion) :nosave-buffer-p t))

(defun buffer-load-from-bookmark (bookmark)
  (buffer-load (url bookmark)))

(defun new-buffer-load-from-bookmark (bookmark)
  "Load a URL in a new buffer (from bookmark)."
  (make-buffer-focus :url (url bookmark)))

(defun new-nosave-buffer-load-from-bookmark (bookmark)
  "Load a URL in a new nosave buffer (from bookmark)."
  (make-buffer-focus :url (url bookmark) :nosave-buffer-p t))

(define-class global-history-source (prompter:source)
  ((prompter:name "Global history")
   ;; REVIEW: Collect history suggestions asynchronously or not?  It's fast
   ;; enough with <10,000 entries on @ambrevar's laptop.
   ;; (prompter:initial-suggestions (history-initial-suggestions))
   (prompter:constructor (lambda (source)
                           (declare (ignorable source))
                           (history-initial-suggestions)))
   (prompter:filter-preprocessor nil)   ; Don't remove non-exact results.
   (prompter:actions '(buffer-load)))
  (:export-class-name-p t))

(define-command set-url (&key (prefill-current-url-p t))
  "Set the URL for the current buffer, completing with history."
  (let ((history (set-url-history *browser*)))
    (when history
      (containers:insert-item history (render-url (url (current-buffer)))))
    (prompt
     :prompt "Open URL"
     :input (if prefill-current-url-p
                (render-url (url (current-buffer))) "")
     :history history
     :sources (list (make-instance 'prompter:raw-source
                                   :name "New URL"
                                   :actions (list (make-unmapped-command buffer-load)
                                                  (make-unmapped-command new-buffer-load)))
                    (make-instance 'global-history-source
                                   ;; TODO: Multi-selection?
                                   :actions (list (make-unmapped-command buffer-load)
                                                  (make-unmapped-command new-buffer-load-from-history)))
                    (make-instance 'bookmark-source
                                   :actions (list (make-unmapped-command buffer-load-from-bookmark)
                                                  (make-unmapped-command new-buffer-load-from-bookmark)))))))

(define-command set-url-new-buffer (&key (prefill-current-url-p t))
  "Prompt for a URL and set it in a new focused buffer."
  (let ((history (set-url-history *browser*)))
    (when history
      (containers:insert-item history (render-url (url (current-buffer)))))
    (prompt
     :prompt "Open URL"
     :input (if prefill-current-url-p
                (render-url (url (current-buffer))) "")
     :history history
     :sources (list (make-instance 'prompter:raw-source
                                   :name "New URL"
                                   :actions (list (make-unmapped-command new-buffer-load)))
                    (make-instance 'global-history-source
                                   :actions (list (make-unmapped-command new-buffer-load-from-history)))
                    (make-instance 'bookmark-source
                                   :actions (list (make-unmapped-command new-buffer-load-from-bookmark)))))))

(define-command set-url-nosave-buffer (&key (prefill-current-url-p t))
  "Prompt for a URL and set it in a new focused nosave buffer."
  (prompt
   :prompt "Open URL"
   :input (if prefill-current-url-p
              (render-url (url (current-buffer))) "")
   :sources (list (make-instance 'prompter:raw-source
                                 :name "New URL"
                                 :actions (list (make-unmapped-command buffer-load)
                                                (make-unmapped-command new-nosave-buffer-load)))
                  (make-instance 'global-history-source
                                 :actions (list (make-unmapped-command buffer-load)
                                                (make-unmapped-command new-nosave-buffer-load-from-history)))
                  (make-instance 'bookmark-source
                                 :actions (list (make-unmapped-command new-nosave-buffer-load-from-bookmark))))))

(defun reload-buffer (&optional (buffer (current-buffer)))
  "Reload a BUFFER or current-buffer if not provided."
  (buffer-load (url buffer) :buffer buffer))

(define-command reload-current-buffer ()
  "Reload current buffer."
  (reload-buffer (current-buffer)))

(define-command reload-buffers ()
  "Reload queried buffer(s)."
  (prompt
   :prompt "Reload buffer(s)"
   :sources (make-instance 'buffer-source
                           :multi-selection-p t
                           :actions (list (make-mapped-command reload-buffer)))))

(define-command switch-buffer-previous ()
  "Switch to the previous buffer in the list of buffers.
That is to say, the one with the most recent access time after the current buffer.
The current buffer access time is set to be the last so that if we keep calling
this command it cycles through all buffers."
  (let* ((buffers (buffer-list :sort-by-time t))
         (last-buffer (alex:last-elt buffers))
         (current-buffer (current-buffer)))
    (when (second buffers)
      (set-current-buffer (second buffers))
      ;; Set the last-access time after switching buffer, since switching
      ;; buffers already sets the slot.
      (setf (last-access current-buffer)
            (local-time:timestamp- (last-access last-buffer) 1 :sec)))))

(define-command switch-buffer-next ()   ; TODO: Rename switch-buffer-oldest
  "Switch to the oldest buffer in the list of buffers."
  (let* ((buffers (buffer-list :sort-by-time t))
         (oldest-buffer (alex:last-elt buffers)))
    (when (eq oldest-buffer (current-buffer))
      ;; Current buffer may already be the oldest, e.g. if other buffer was
      ;; opened in the background.
      (setf oldest-buffer (or (second (nreverse buffers))
                              oldest-buffer)))
    (set-current-buffer oldest-buffer)))

(export-always 'mode-name)
(defun mode-name (mode)
  (class-name (original-class mode)))

(export-always 'disable-modes)
(defun disable-modes (modes &optional (buffer (current-buffer)))
  "Disable MODES for BUFFER.
MODES should be a list symbols, each possibly returned by `mode-name'."
  (dolist (mode (uiop:ensure-list modes))
    (let ((command (mode-command mode)))
      (if command
          (funcall command :buffer buffer :activate nil)
          (log:warn "Mode command ~a not found." mode)))))

(export-always 'enable-modes)
(defun enable-modes (modes &optional (buffer (current-buffer)) args)
  "Enable MODES for BUFFER.
MODES should be a list of symbols, each possibly returned by `mode-name'.
ARGS are passed to the mode command."
  (dolist (mode (uiop:ensure-list modes))
    (let ((command (mode-command mode)))
      (if command
          (apply #'funcall command :buffer buffer :activate t args)
          (log:warn "Mode command ~a not found." mode)))))

(export-always 'active-modes)
(defun active-modes (&optional (buffer (current-buffer)))
  "Return the enabled modes of BUFFER.
The modes are the mode symbols, as returned by `mode-name'."
  (mapcar (lambda (mode) (mode-name mode))
          (modes buffer)))

(define-class active-mode-source (prompter:source)
  ((prompter:name "Active modes")
   (buffers :initarg :buffers :accessor buffers :initform nil)
   (prompter:multi-selection-p t)
   (prompter:constructor (lambda (source)
                           (delete-duplicates
                            (alex:mappend
                             #'modes
                             (uiop:ensure-list (buffers source)))
                            :test (lambda (i y) (equal (mode-name i)
                                                       (mode-name y)))))))
  (:export-class-name-p t))

(define-class inactive-mode-source (prompter:source)
  ((prompter:name "Inactive modes")
   (buffers :initarg :buffers :accessor buffers :initform nil)
   (prompter:multi-selection-p t)
   (prompter:constructor (lambda (source)
                           (let ((common-modes
                                   (reduce #'intersection
                                           (mapcar (lambda (b)
                                                     (mapcar #'mode-name (modes b)))
                                                   (uiop:ensure-list (buffers source))))))
                             (set-difference (mode-list) common-modes)))))
  (:export-class-name-p t))

(define-command disable-mode-for-current-buffer (&key (buffer (current-buffer)))
  "Disable queried mode(s)."
  (let ((modes (prompt
                :prompt "Disable mode(s)"
                :sources (make-instance 'active-mode-source
                                        :buffers buffer))))
    (disable-modes (mapcar #'mode-name modes) buffer)))


(define-command disable-mode-for-buffer ()
  "Disable queried mode(s) for select buffer(s)."
  (let* ((buffers (prompt
                   :prompt "Disable mode(s) for buffer(s)"
                   :sources (make-instance 'buffer-source
                                           :multi-selection-p t
                                           :actions '())))
         (modes (prompt
                 :prompt "Disable mode(s)"
                 :sources (make-instance 'active-mode-source
                                         :buffers buffers))))
    (loop for buffer in buffers
          do (disable-modes (mapcar #'mode-name modes) buffer))))

(define-command enable-mode-for-current-buffer (&key (buffer (current-buffer)))
  "Enable queried mode(s)."
  (let ((modes (prompt
                :prompt "Enable mode(s)"
                :sources (make-instance 'inactive-mode-source
                                        :buffers buffer))))
    (enable-modes (uiop:ensure-list modes) buffer)))

(define-command enable-mode-for-buffer ()
  "Enable queried mode(s) for select buffer(s)."
  (let* ((buffers (prompt
                   :prompt "Enable mode(s) for buffer(s)"
                   :sources (make-instance 'buffer-source
                                           :multi-selection-p t
                                           :actions '())))
         (modes (prompt
                 :prompt "Enable mode(s)"
                 :sources (make-instance 'inactive-mode-source
                                         :buffers buffers))))
    (loop for buffer in buffers
          do (enable-modes (uiop:ensure-list modes) buffer))))

(define-command open-inspector ()
  "Open the inspector, a graphical tool to inspect and change the content of the buffer."
  (ffi-inspector-show (current-buffer)))

(export-always 'print-buffer)
(define-command print-buffer ()
  "Print the current buffer."
  (pflet ((print-buffer () (print)))
         (print-buffer)))

(define-command focus-first-input-field (&key (type-blacklist '("hidden"
                                                                "checkbox"
                                                                "button")))
  "Move the focus to the first input field of `buffer'."
  ;; The list of input types can be found below.
  ;; https://developer.mozilla.org/en-US/docs/Web/HTML/Element/input
  ;; TODO: The following results in 2 DOM traversal.  We should probably do the
  ;; whole thing in a single Parenscript instead.
  (pflet ((nth-input-type (i) (ps:chain document
                                        (get-elements-by-tag-name "INPUT")
                                        (item (ps:lisp i))
                                        type))
          (nth-input-focus (i) (ps:chain document
                                         (get-elements-by-tag-name "INPUT")
                                         (item (ps:lisp i))
                                         (focus))))
    (nth-input-focus (do ((i 0 (1+ i)))
                         ((notany
                           (lambda (type) (equalp (nth-input-type i) type))
                           type-blacklist)
                          i)))))
