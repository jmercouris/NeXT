;;;; SPDX-FileCopyrightText: Atlas Engineer LLC
;;;; SPDX-License-Identifier: BSD-3-Clause

(in-package :nyxt)

;; A URL is defined by the following components:
;; URL = scheme:[//authority]path[?query][#fragment]

(export-always 'render-url)
(declaim (ftype (function ((or quri:uri string)) string) render-url))
(defun render-url (url)
    "Return decoded URL.
If the URL contains hexadecimal-encoded characters, return their unicode counterpart."
  (let ((url (if (stringp url)
                 url
                 (quri:render-uri url))))
    (the (values (or string null) &optional)
         (or (ignore-errors (ffi-display-uri url))
             url))))

(export-always 'generate-search-query)
(defun generate-search-query (search-string search-url)
  (let* ((encoded-search-string
           ;; We need to encode the search string to escape special characters.
           ;; Besides, we separate search patterns by a "+".
           (cl-ppcre:regex-replace-all "(%20)+" (quri:url-encode search-string) "+"))
         (url (format nil search-url encoded-search-string)))
    url))

(export-always 'valid-url-p)
(defun valid-url-p (url)
  ;; List of URI schemes: https://www.iana.org/assignments/uri-schemes/uri-schemes.xhtml
  ;; Last updated 2020-08-26.
  (let* ((nyxt-schemes '("lisp" "javascript"))
         (iana-schemes
           '("aaa" "aaas" "about" "acap" "acct" "cap" "cid" "coap" "coap+tcp" "coap+ws"
             "coaps" "coaps+tcp" "coaps+ws" "crid" "data" "dav" "dict" "dns" "example" "file"
             "ftp" "geo" "go" "gopher" "h323" "http" "https" "iax" "icap" "im" "imap" "info"
             "ipp" "ipps" "iris" "iris.beep" "iris.lwz" "iris.xpc" "iris.xpcs" "jabber"
             "ldap" "leaptofrogans" "mailto" "mid" "msrp" "msrps" "mtqp" "mupdate" "news"
             "nfs" "ni" "nih" "nntp" "opaquelocktoken" "pkcs11" "pop" "pres" "reload" "rtsp"
             "rtsps" "rtspu" "service" "session" "shttp" "sieve" "sip" "sips" "sms" "snmp"
             "soap.beep" "soap.beeps" "stun" "stuns" "tag" "tel" "telnet" "tftp"
             "thismessage" "tip" "tn3270" "turn" "turns" "tv" "urn" "vemmi" "vnc" "ws" "wss"
             "xcon" "xcon-userid" "xmlrpc.beep" "xmlrpc.beeps" "xmpp" "z39.50r" "z39.50s"))
         (valid-schemes (append nyxt-schemes iana-schemes))
         (uri (ignore-errors (quri:uri url))))
    (flet ((hostname-found-p (name)
             (handler-case (iolib/sockets:lookup-hostname name)
               (t () nil)))
           (valid-scheme-p (scheme)
             (find scheme valid-schemes :test #'string=))
           (http-p (scheme)
             (find scheme '("http" "https") :test #'string=)))
      (and uri
           (quri:uri-p uri)
           (valid-scheme-p (quri:uri-scheme uri))
           ;; `parse-url' tries to guess
           ;; the URL from the user input by prefixing it with HTTPS:
           (or (not (http-p (quri:uri-scheme uri)))
               (and
                ;; "http://" does not have a host.
                ;; A valid URL may have an empty domain, e.g. http://192.168.1.1.
                (quri:uri-host uri)
                ;; "http://algo" has the "algo" hostname but it's probably invalid
                ;; unless it's found on the local network.  We also need to
                ;; support "localhost" and the current system hostname.
                ;; get-host-by-name may signal a ns-try-again-condition which is
                ;; not an error, so we can't use `ignore-errors' here.
                (or (quri:ip-addr-p (quri:uri-host uri))
                    (hostname-found-p (quri:uri-host uri)))))))))

(declaim (ftype (function (t) quri:uri) ensure-url))
(defun ensure-url (thing)
  "Return `quri:uri' derived from THING.
If it cannot be derived, return an empty `quri:uri'."
  (the (values quri:uri &optional)
       (if (quri:uri-p thing)
           thing
           (or (ignore-errors (quri:uri thing))
               (quri:uri "")))))

(declaim (ftype (function ((or quri:uri string null)) boolean) url-empty-p))
(export-always 'url-empty-p)
(defun url-empty-p (url)
  "Small convenience function to check whether the given URL is empty."
  (the (values boolean &optional)
       (uiop:emptyp (if (quri:uri-p url) (quri:render-uri url) url))))

(declaim (ftype (function (quri:uri) boolean)
                empty-path-url-p host-only-url-p))
(export-always 'empty-path-url-p)
(defun empty-path-url-p (url)
  (or (string= (quri:uri-path url) "/")
      (null (quri:uri-path url))))

(export-always 'host-only-url-p)
(defun host-only-url-p (url)
  (every #'null
         (list (quri:uri-query url)
               (quri:uri-fragment url)
               (quri:uri-userinfo url))))

(declaim (ftype (function (quri:uri quri:uri) boolean) schemeless-uri=))
(defun schemeless-uri= (uri1 uri2)
  "Like `quri:uri=' but ignore scheme in comparison.
Authority is compared case-insensitively (RFC 3986)."
 (and (equal  (or (quri:uri-path uri1) "/") (or (quri:uri-path uri2) "/"))
      (equal  (quri:uri-query uri1)     (quri:uri-query uri2))
      (equal  (quri:uri-fragment uri1)  (quri:uri-fragment uri2))
      (equalp (quri:uri-authority uri1) (quri:uri-authority uri2))))

(declaim (ftype (function (quri:uri) string) schemeless-url))
(defun schemeless-url (uri)             ; Inspired by `quri:render-uri'.
  "Return URL without its scheme (e.g. it removes 'https://')."
  ;; Warning: We can't just set `quri:uri-scheme' to nil because that would
  ;; change the port (e.g. HTTP defaults to 80, HTTPS to 443).
  (format nil
          "~@[~A~]~@[~A~]~@[?~A~]~@[#~A~]"
          (quri:uri-authority uri)
          (or (quri:uri-path uri) "/")
          (quri:uri-query uri)
          (quri:uri-fragment uri)))

(declaim (ftype (function (quri:uri quri:uri) boolean) url<))
(defun uri< (uri1 uri2)
  "Like `string<' but ignore the URI scheme.
This way, HTTPS and HTTP is ignored when comparing URIs."
  (string< (schemeless-url uri1)
           (schemeless-url uri2)))

(declaim (ftype (function (string &rest string) (function (quri:uri) boolean))
                match-scheme match-host match-domain
                match-file-extension match-regex match-url))
(export-always 'match-scheme)
(defun match-scheme (scheme &rest other-schemes)
  "Return a predicate for URLs matching one of SCHEME or OTHER-SCHEMES."
  #'(lambda (url)
      (some (alex:curry #'string= (quri:uri-scheme url))
            (cons scheme other-schemes))))

(export-always 'match-host)
(defun match-host (host &rest other-hosts)
  "Return a predicate for URLs matching one of HOST or OTHER-HOSTS."
  #'(lambda (url)
      (some (alex:curry #'string= (quri:uri-host url))
            (cons host other-hosts))))

(export-always 'match-domain)
(defun match-domain (domain &rest other-domains)
  "Return a predicate for URLs matching one of DOMAIN or OTHER-DOMAINS."
  #'(lambda (url)
      (some (alex:curry #'string= (quri:uri-domain url))
            (cons domain other-domains))))

(export-always 'match-file-extension)
(defun match-file-extension (extension &rest other-extensions)
  "Return a predicate for URLs matching one of EXTENSION or OTHER-EXTENSIONS."
  #'(lambda (url)
      (some (alex:curry #'string= (pathname-type (or (quri:uri-path url) "")))
            (cons extension other-extensions))))

(export-always 'match-regex)
(defun match-regex (regex &rest other-regex)
  "Return a predicate for URLs matching one of REGEX or OTHER-REGEX."
  #'(lambda (url)
      (some (alex:rcurry #'cl-ppcre:scan (render-url url))
            (cons regex other-regex))))

(export-always 'match-url)
(defun match-url (one-url &rest other-urls)
  "Return a predicate for URLs exactly matching ONE-URL or OTHER-URLS."
  #'(lambda (url)
      (some (alex:rcurry #'string= (render-url url))
            (mapcar (lambda (u) (quri:url-decode u :lenient t))
                    (cons one-url other-urls)))))

(declaim (ftype (function (quri:uri quri:uri) boolean) url-equal))
(defun url-equal (url1 url2)
  "URLs are equal if the URIs are equal, scheme excluded.
Empty paths are also excluded from the comparison.
For instance, these are equal:
- http://example.org
- https://example.org/"
  (if (and (quri:uri-http-p url1)
           (quri:uri-http-p url2))
      (schemeless-uri= url1 url2)
      (the (values boolean &optional) (quri:uri= url1 url2))))

(declaim (ftype (function (quri:uri quri:uri list) boolean) eq-uri-p))
(defun eq-uri-p (url1 url2 eq-fn-list)
  "Return non-nil when URL1 and URL2 are \"equal\" as dictated by EQ-FN-LIST.

EQ-FN-LIST is a list of functions that take URL1 and URL2 as arguments and
return a boolean.  It defines an equivalence relation induced by EQ-FN-LIST.
`quri:uri=', `url-equal' and `schemeless-uri=' are examples of equivalence
relations."
  ;; (and (fn1 url1 url2) (fn2 url1 url2) ...) stops as soon as any fn returns
  ;; nil, unlike the solution below.
  (every #'identity (mapcar (lambda (fn) (funcall fn url1 url2)) eq-fn-list)))

(declaim (ftype (function (quri:uri quri:uri) boolean)
                distinct-url-path-p scheme= domain= host=))
(defun distinct-url-path-p (url1 url2)
  "Return non-nil when URL1 and URL2 have distinct paths."
  ;; See https://github.com/fukamachi/quri/issues/48.
  (not (equalp (string-right-trim "/" (or (quri:uri-path url1) ""))
               (string-right-trim "/" (or (quri:uri-path url2) "")))))

(defun scheme= (url1 url2)
  "Return non-nil when URL1 and URL2 have the same scheme.
HTTP and HTTPS belong to the same equivalence class."
  (or (equalp (quri:uri-scheme url1) (quri:uri-scheme url2))
      (and (quri:uri-http-p url1) (quri:uri-http-p url2))))

(defun domain= (url1 url2)
  "Return non-nil when URL1 and URL2 have the same domain."
  (equalp (quri:uri-domain url1) (quri:uri-domain url2)))

(defun host= (url1 url2)
  "Return non-nil when URL1 and URL2 have the same host.
This is a more restrictive requirement than `domain='."
  ;; What's the difference between quri:uri-host and quri:uri-authority?
  (equalp (quri:uri-host url1) (quri:uri-host url2)))

(declaim (ftype (function (string) (values (or quri:uri null) t &optional)) parse-url))
(defun parse-url (input-url)
  "From user input, return the full URL to visit.

If the first word references a search engine, generate a search query.
If the input starts with an URI scheme, open it as is.
If the input is actually a file path, open it.
Suppose the user omitted the scheme: if the input prefixed by 'https://' gives a valid URI, go to it.
Otherwise, build a search query with the default search engine."
  (let* ((search-engines (all-search-engines))
         (terms (str:split " " input-url :omit-nulls t))
         (engine (find (first terms)
                       search-engines :test #'string= :key #'shortcut)))
    (if engine
        (let ((new-input (str:join " " (rest terms))))
          (if (and (not (str:emptyp (fallback-url engine)))
                   (str:emptyp new-input))
              (quri:uri (fallback-url engine))
              (quri:uri (generate-search-query new-input (search-url engine)))))
        (let ((recognized-scheme (and (valid-url-p input-url)
                                      (quri:uri-scheme (quri:uri input-url)))))
          (cond
            ((and recognized-scheme
                  (not (string= "file" recognized-scheme)))
             (quri:uri input-url))
            ((or (string= "file" recognized-scheme)
                 (uiop:file-exists-p input-url))
             (quri:uri (if (string= "file" recognized-scheme)
                           input-url
                           (format nil "file://~a"
                                   (uiop:ensure-absolute-pathname
                                    input-url *default-pathname-defaults*)))))
            ((valid-url-p (str:concat "https://" input-url))
             (quri:uri (str:concat "https://" input-url)))
            (t (alex:if-let ((default (default-search-engine search-engines)))
                 (quri:uri (generate-search-query input-url (search-url default)))
                 (quri:uri input-url))))))))

(export-always 'lisp-url)
(declaim (ftype (function (t &rest t) string) lisp-url))
(defun lisp-url (lisp-form &rest more-lisp-forms)
  "Generate a lisp:// URL from the given Lisp forms. This is useful for encoding
functionality into internal-buffers."
  (the (values string &optional)
       (apply #'str:concat "lisp://"
              (mapcar (alex:compose #'quri:url-encode #'write-to-string)
                      (cons lisp-form more-lisp-forms)))))

;; TODO How to pass the optional arg to fetch-links?
(declaim (ftype (function (quri:uri) list) renderer-fetch-links))
(defun renderer-fetch-links (url)
  "Return a list of links from URL.
If there is a buffer corresponding to URL, then fetch the links from it.
Otherwise, create a new buffer to fetch the links."
  (let ((buffer-exists-p (first (buffer-list :url url))))
    (if buffer-exists-p
        (funcall #'fetch-links buffer-exists-p)
        ;; Buffers should be made as light as possible (no images, CSS, etc).
        (let ((buffer (make-nosave-buffer :modes '(web-mode noimage-mode)
                                          :url url))
              (channel (make-channel)))
          (sera:add-hook (buffer-loaded-hook buffer)
								         (make-handler-buffer
                          (lambda (buffer)
                            (calispel:! channel (funcall #'fetch-links buffer))
                            ;; The buffer is only needed to get the links.
                            ;; Alternative:
                            ;; (sera:remove-hook (buffer-loaded-hook buffer)
                            ;;                   'fetch-links)
                            (buffer-delete buffer))
                          :name 'fetch-links))
          (calispel:? channel)))))

(declaim (ftype (function (quri:uri &optional list) list) http-fetch-links))
(defun http-fetch-links (url &optional (filtering-rules (list #'host=
                                                              #'distinct-url-path-p
                                                              #'scheme=)))
  "Return a list of links from URL."
  ;; TODO
  ;; quri:merge-uris bug - see https://github.com/fukamachi/quri/issues/47;
  ;; Manage urls that download stuff;
  ;; Manage urls that can't be reached.
  (let ((links
          (remove
           nil
           (mapcar
            (lambda (tag) (unless (null (plump:attribute tag "href"))
                       (quri:merge-uris (plump:attribute tag "href") url)))
            (plump:get-elements-by-tag-name (plump:parse (dex:get url)) "a")))))
    (loop for link in links
          when (eq-uri-p link url filtering-rules)
            collect link)))

(declaim (ftype (function (integer
                           list
                           &optional (or compiled-function symbol)
                           (or compiled-function symbol))
                          list)
                recursive-links))
(defun recursive-links (depth url-list
                        &optional
                          (fetch-links-fn #'http-fetch-links)
                          (filter-links-fn #'identity))
  "Return a list of length DEPTH.

Each element is a list of URLs at distance 0,1,...,DEPTH from URL-LIST.

DEPTH is the minimum distance between 2 URLs, i.e. how many URLs need to be
visited to get from one to the other via their links.

URL-LIST is list of URLs of type QURI.URI.

FILTER-LINKS-FN is a function that takes a list of URLs and returns another.

FETCH-LINKS-FN is a function that takes a URL and returns its corresponding
links.

Example: Take URL-LIST to be a list of one element, url U, and DEPTH to be 2.
The links of U are collected in a list, (list (\"links-of\" U)).  With that list
as the new starting point, the same method is called with depth 1, and finally
with depth 0.  Find below the stack of calls.

(recursive-links 2 (list U))
(recursive-links 1 (list (\"links-of\" U)))
(recursive-links 0 (list (\"links-of\" (\"links-of\" U))))

-> (list ((list U)                                 ; DEPTH 0
          (list (\"links-of\" U))                  ; DEPTH 1
          (list (\"links-of\" (\"links-of\" U))))) ; DEPTH 2"
  (labels ((nth-power-function (n arg fn)
             "Apply FN on ARG at most N times, as long as ARG is non-nil."
             (cond ((or (zerop n) (null arg)) arg)
                   (t (nth-power-function (1- n) (funcall fn arg) fn))))
           (url-in-list-p (url list)
             "Return non-nil when URL is a member of LIST."
             (position url list :test #'quri:uri=)))
    (let ((url-list-all-depth (when url-list (list url-list)))
          ;; Unless `filter-links-fn' is the identity function, VISITED-URL-LIST
          ;; doesn't replace URL-LIST-ALL-DEPTH.
          (visited-url-list (when url-list url-list)))
      (nth-power-function
       depth
       url-list
       (lambda (url-list)
         ;; Guarantee that the links of any URL are fetched once and only once;
         ;; and that LINK-LIST is a set, i.e. a list without duplicated
         ;; elements.
         (let ((link-list))
           (dolist (url url-list)
             (loop for link in (funcall fetch-links-fn url)
                   unless (or (url-in-list-p link link-list)
                              (url-in-list-p link visited-url-list))
                     do (progn (push link link-list)
                               (push link visited-url-list))))
           (push (funcall filter-links-fn link-list) url-list-all-depth)
           link-list)))
      (nreverse url-list-all-depth))))

(defun interactive-recursive-links (depth url-list
                                    &optional (fetch-links-fn #'http-fetch-links))
  "Return the same as `recursive-links'.
At each DEPTH, a subset of URLs is selected with `prompt-buffer'."
  (recursive-links
   depth
   url-list
   fetch-links-fn
   (lambda (url-list) (prompt :prompt "Choose URLs"
                         :sources (make-instance 'prompter:source
                                                 :constructor url-list
                                                 :name "URL"
                                                 :multi-selection-p t)))))
