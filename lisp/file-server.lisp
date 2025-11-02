(in-package :file-server)

;; Database
(defparameter *db-host* (u:getenv "DB_HOST" :default "localhost"))
(defparameter *db-port* (u:getenv "DB_PORT" :default 5432 :type :integer))
(defparameter *db-name* (u:getenv "DB_NAME" :default "fileserver"))
(defparameter *db-username* (u:getenv "DB_USER" :default "fileserver"))
(defparameter *db-password* (u:getenv "DB_PASSWORD"
                              :default "fileserver-password"))

;; User
(defparameter *root-username* (u:getenv "ROOT_USER" :default "admin"))
(defparameter *root-password* (u:getenv "ROOT_PASSWORD"
                                :default "admin-password-1234"))
(defparameter *root-role* "admin")
(defparameter *guest-username* "guest")
(defparameter *guest-password* "guest-password-1234")
(defparameter *guest-role* "guest")

;; JWT Secret
(defparameter *jwt-secret*
  (b:string-to-octets (u:getenv "JWT_SECRET" :default "32-char secret")))

;; Javascript
(defparameter *web-directory* (u:getenv "WEB_DIRECTORY"
                               :default "/app/web"))
(defparameter *file-server-js* (u:join-paths *web-directory* "file-server.js"))
(defparameter *favicon* (u:join-paths *web-directory* "favicon.ico"))

;; HTTP and Swank servers
(defparameter *http-port* (u:getenv "HTTP_PORT" :default 8080 :type :integer))
(defparameter *document-root* (u:getenv "FS_DOCUMENT_ROOT"
                                :default "/app/shared-files/"))
(defparameter *swank-port* (u:getenv "SWANK_PORT" :default 4005 :type :integer))

;; Logs
(defparameter *log-file* (or (u:getenv "LOG_FILE") *standard-output*))
(defparameter *log-severity-threshold*
  (intern (string-upcase (or (u:getenv "LOG_SEVERITY") "DEBUG")) :keyword))
(defparameter *log-suppress-health*
  (u:getenv "LOG_SUPPRESS_HEALTH" :type :boolean :default t))

;; Other
(defparameter *http-server* nil)
(defparameter *swank-server* nil)
(defparameter *root-userid* nil)
(defparameter *rbac* nil)
(defparameter *directory-syncing* t)

(defun issue-jwt (user-id &optional (expiration-seconds 3600))
  "Issue a JWT for a user."
  (let* ((claims `(("sub" . ,user-id)
                    ("exp" . ,(+ (u:universal-time-to-unix-time)
                                expiration-seconds)))))
    (j:encode :hs256 *jwt-secret* claims)))

(defun validate-jwt (token)
  "Validate a JWT token. Returns username if JWT token validates and user
exists. Otherwise, logs a message and returns NIL."
  (handler-case
    (multiple-value-bind (claims headers sig)
      (jose:decode :hs256 *jwt-secret* token)
      (declare (ignore headers sig))
      (when claims
        (let ((user-id (cdr (assoc "sub" claims :test #'string=))))
          (if user-id
            (let ((user (handler-case
                          (a:get-value *rbac* "users" "username"
                            "id" user-id)
                          (error (e)
                            (u:log-it
                              :warn
                              "JWT has invalid user ID: ~a" e)
                            nil))))
              (if user
                user
                (progn
                  (u:log-it :warn "User with ID ~a not found" user-id)
                  nil)))
            (progn
              (u:log-it :warn "User ID not found in JWT")
              nil)))))
    (error (e)
      (u:log-it :warn "Invalid JWT: ~a" e)
      nil)))

;;
;; Custom Hunchentoot acceptor, for log-it logging
;;

(defclass fs-acceptor (h:easy-acceptor)
  ())

(defmethod h:acceptor-log-access ((acceptor fs-acceptor) &key return-code)
  "Override to route access logs through u:log-it."
  (let* ((code (h:return-code*))
          (uri (h:request-uri*))
          (health-log (equal uri "/health"))
          (log-severity (cond
                          (health-log :debug)
                          ((< code 300) :info)
                          ((< code 500) :warn)
                          (t :error))))
    (unless (and health-log *log-suppress-health*)
      (u:log-it-pairs log-severity
        :type "access"
        :remote (h:remote-addr*)
        :server (h:local-addr*)
        :host (h:host)
        :method (h:request-method*)
        :uri uri
        :return-code code
        :status return-code
        :content-length (or (h:content-length*) 0)
        :referer (h:referer)
        :agent (h:user-agent)))))

(defmethod h:acceptor-log-message ((acceptor fs-acceptor)
                                    log-level
                                    format-string &rest format-arguments)
  (let* ((log-severity (case log-level
                         (:error :error)
                         (:warning :warn)
                         (:info :info)
                         (t :debug)))
          (params (append (list log-severity format-string) format-arguments)))
    (apply #'u:log-it params)))
;;
;; End custom Hunchentoot acceptor
;;

(defun invert-hex-color (hex-string)
  "Return the inverse color of the color represented by HEX-STRING. HEX-STRING
is a 3-digit or 6-digit hexadecimal value that is optionally prefixed by the
# symbol. Examples of valid values for HEX-STRING: #00FF33, #FFF, AA77CE, 123.
This function returns the inverted value using the same number of hexadecimal
digits provided in HEX-STRING, but always prefixes the return value with the
# symbol. If HEX-STRING is invalid, this function raises an error."
  (let* ((color (string-trim "# " hex-string))
          (short (= (length color) 3))
          (full-color (if short
                        (loop for char across color
                          append (list char char) into full
                          finally (return (map 'string 'identity full)))
                        color))
          (components (loop for a from 0 to 4 by 2
                        for b = (+ a 2)
                        collect
                        (parse-integer (subseq full-color a b) :radix 16)))
          (inverse-components (loop for component in components
                                collect (- 255 component)))
          (hex-components (loop for value in inverse-components
                            collect (format nil "~2,'0x" value)))
          (final-components (if short
                              (mapcar
                                (lambda (c) (subseq c 0 1))
                                hex-components)
                              hex-components)))
    (format nil "#~{~a~}" final-components)))

(defun db-directory-id (directory)
  "Determines if DIRECTORY exists as a resource in the database, returning the
directory's ID if it does and NIL otherwise."
  (a:get-id *rbac* "resources" directory))

(defun readable-timestamp (universal-time)
  "Return UNIVERSAL-TIME formatted as a timestamp that reads like this:
YYYY-MM-DD HH:MM. If UNIVERSAL-TIME is NIL or :NULL, this function returns an
empty string."
  (if (and universal-time (not (eql universal-time :null)))
    (let ((ts (u:timestamp-string :universal-time universal-time)))
      (subseq (re:regex-replace "T" ts " ") 0 16))
    ""))

(defun db-user-id (user password)
  (a:d-login *rbac* user password))

(defun db-list-roles (user &optional all-roles)
  (when user
    (handler-case
      (if all-roles
        (a:list-user-role-names *rbac* user :page-size 1000)
        (a:list-user-role-names-regular *rbac* user :page-size 1000))
      (error (e)
        (u:log-it-pairs :error
          :detail (format nil "Failed to retrieve roles for user '~a'" user)
          :error (format nil "~a" e))))))

(defun db-list-directories ()
  (a:list-resource-names *rbac*))

(defun fs-list-directories ()
  (let* ((dirs (mapcar
                 (lambda (d) (format nil "~a" d))
                 (directory (format nil "~a**/" *document-root*))))
          (l (1- (length (u:root-path dirs)))))
    (when dirs
      (sort
        (mapcar (lambda (dir) (subseq dir l)) dirs)
        #'string<))))

(defun hash-directory-list (directory-list)
  (if directory-list
    (u:hash-string (format nil "~{~a~^|~}" directory-list))
    ""))

(defun alist-to-hashtable (alist &key collect)
  (if collect
    (loop with h = (make-hash-table :test 'equal)
      for (key . value) in alist
      for existing = (gethash key h)
      when (null existing) do
      (let ((array (setf (gethash key h)
                     (make-array 100 :adjustable t :fill-pointer 0))))
        (vector-push-extend value array))
      else do
      (vector-push-extend value existing)
      finally (return h))
    (loop with h = (make-hash-table :test 'equal)
      for (key . value) in alist
      do (setf (gethash key h) value)
      finally (return h))))

(defun sync-directories ()
  "Ensures that directories that have been added to the file system are added to
the RBAC database, and that directories that have been removed from the file
system are removed from the RBAC database. The resources in the RBAC database
should correspond exactly to the directories in the file system."
  (u:log-it :debug "Syncing directories")
  (let* ((fs-dirs (fs-list-directories))
          (db-dirs (db-list-directories))
          (added (loop
                   for dir in fs-dirs
                   unless (db-directory-id dir)
                   do (a:d-add-resource *rbac* dir :roles (list *root-role*))
                   and collect dir))
          (removed (loop for dir in db-dirs
                     unless (member dir fs-dirs :test 'equal)
                     do (a:d-remove-resource *rbac* dir)
                     and collect dir)))
    (when added
      (u:log-it :info "added directories: ~{~a~^, ~}" added))
    (when removed
      (u:log-it :info "removed directories: ~{~a~^, ~}" removed))))

(defun clean-path (path)
  "Returns the path portion of PATH, which must be a string that starts with a
slash. If PATH points to a directory, then this function adds the trailing slash
if necessary. Otherwise, if PATH points to a file, this function removes the
file name and returns the path to the file with a trailing slash."
  (if (equal path "/")
    path
    (let* ((abs-path (u:join-paths *document-root* path))
            (path-only (if (eql (u:path-type abs-path) :directory)
                         path
                         (u:path-only path)))
            (clean-path (if (equal path-only "/")
                          "/"
                          (format nil "/~a/" (string-trim "/" path-only)))))
      (u:log-it :debug "path-only=~a; clean-path=~a" path clean-path)
      clean-path)))

(defun has-read-access (user path)
  (u:log-it-pairs :debug :details "Checking access"
    :user user :permission "read" :path path)
  (a:user-allowed *rbac* user "read" path))

(defun list-files (abs-path)
  (let ((path (if (re:scan "/$" abs-path)
                abs-path
                (format nil "~a/" abs-path))))
    (mapcar
      (lambda (p)
        (subseq (namestring p) (1- (length *document-root*))))
      (uiop:directory-files path))))

(defun rdl-subdirectories (user abs-path)
  (let ((path (if (re:scan "/$" abs-path)
                abs-path
                (format nil "~a/" abs-path))))
    (remove-if-not
      (lambda (path)
        (has-read-access user path))
      (mapcar
        (lambda (p)
          (subseq (namestring p) (1- (length *document-root*))))
        (uiop:subdirectories path)))))

(defun join-html (list &optional new-lines)
  (let ((format-string (if new-lines "~{~a~%~}" "~{~a~}")))
    (format nil format-string list)))

(defun add-to-url-query (path &rest pairs)
  (when path
    (loop
      for key in pairs by #'cddr
      for value in (cdr pairs) by #'cddr
      for beg = path then url
      for url = (if (and key value)
                  (let ((format-string (cond
                                         ((re:scan "[?]$" beg) "~a~(~a~)=~a")
                                         ((re:scan "[?]" beg) "~a&~(~a~)=~a")
                                         (t "~a?~(~a~)=~a"))))
                    (format nil format-string
                      beg key
                      (h:url-encode (format nil "~a" value))))
                  (or beg url))
      finally (return url))))

(defun menu (user subtitle)
  (let* ((roles (db-list-roles user t))
          (is-admin (member *root-role* roles :test 'equal)))
    (s:with-html-string
      (:nav :class "navbar"
        (:ul :class "nav-menu"
          (:li (:a :href "/files" "files"))
          (when is-admin
            (:li (:a :href "/list-users" "list users")))
          (if user
            (:li (:a :href "/logout"
                   (:img :src "/image?name=user.png")
                   (:span :class "user" user)
                   (:span :class "login"
                     (if (equal user *guest-username*)
                       "[log in]"
                       "[log out]"))))
            (unless (equal subtitle "Log In")
              (:li (:a :href "/logout" "log in")))))))))

(defun page (content &key subtitle user)
  (u:log-it-pairs :debug :details "Rendering page" :subtitle subtitle :user user)
  (let ((title "Donnie's Bad-Ass File Server"))
    (s:with-html-string
      (:doctype)
      (:html
        (:head
          (:title title)
          (:link :rel "stylesheet" :href "/css"))
        (:body
          (:div :class "main-page"
            (:raw (menu user subtitle))
            (:div :class "title" title)
            (when subtitle (:h2 subtitle))
            (:raw content)))))))

(defun assemble-breadcrumbs (path)
  (loop
    with path-parts = (cons "/"
                        (re:split "/" (string-trim "/" path)))
    with count = (length path-parts)
    for part in path-parts
    for index = 1 then (1+ index)
    for last = (= index count)
    for parent-path = part then (u:join-paths parent-path part)
    for parent-name = "root" then part
    collect (if last
              (format nil "~a" parent-name)
              (s:with-html-string
                (:a :href (format nil "?path=~a" parent-path)
                  parent-name)))
    into breadcrumbs
    finally (return (format nil "~{~a~^/~}" breadcrumbs))))

(defun assemble-access-list (path)
  (let* ((count-list 5)
          (count-total 20)
          (access-list (a:list-resource-usernames
                         *rbac* path nil :page-size count-total))
          (count-actual (length access-list))
          (access-list-show (if (<= (length access-list) count-list)
                              access-list
                              (subseq access-list 0 count-list)))
          (additional (cond
                        ((<= count-actual count-list) nil)
                        ((< count-actual count-total)
                          (format nil "and ~d more"
                            (- count-total count-actual)))
                        (t "and many more"))))
    (when additional
      (setf access-list-show
        (append access-list-show (list additional))))
    access-list-show))

(defun render-directory-listing (user path abs-path)
  (setf (h:content-type*) "text/html")
  (let ((files (list-files abs-path))
         (subdirs (rdl-subdirectories user abs-path))
         (crumbs (assemble-breadcrumbs path))
         (access-list (assemble-access-list path)))
    (u:log-it-pairs :debug
      :details "List directories"
      :files (map 'vector 'identity files)
      :subdirectories (map 'vector 'identity subdirs)
      :access-list (map 'vector 'identity access-list))
    (page (s:with-html-string
            (:div :class "breadcrumb"
              (:img :src "/image?name=home.png" :width 24 :height 24)
              (:div (:raw crumbs)))
            (:div :class "access-list"
              (:img :src "/image?name=users.png" :width 16 :height 16)
              (:span (format nil "~{~a~^, ~}" access-list)))
            ;; Directories
            (:ul :class "listing"
              (mapcar
                (lambda (d)
                  (:li (:a :href (format nil "/files?path=~a" d)
                         (:img :src "/image?name=folder.png"
                           :width 24 :height 24)
                         (u:leaf-directory-only d))))
                subdirs))
            ;; Files
            (:ul :class "listing"
              (mapcar
                (lambda (f)
                  (:li (:a :href (format nil "/files?path=~a" f)
                         :target "_blank"
                         (:img :src "/image?name=file.png"
                           :width 24 :height 24)
                         (u:filename-only f))))
                files)))
      :user user
      :subtitle "Files")))

(defmethod h:acceptor-log-message ((acceptor h:easy-acceptor)
                                    log-level
                                    (format-string string)
                                    &rest format-arguments)
  (let ((log-severity (case log-level
                        (:error :error)
                        (:warning :warn)
                        (:info :info)
                        (otherwise :debug))))
    (funcall #'u:log-it log-severity format-string format-arguments)))

(h:define-easy-handler (health :uri "/health") ()
  (format nil "<html><body><h1>OK</h1>~a</body></html>~%"
    (u:timestamp-string)))

(h:define-easy-handler (login-handler :uri "/login")
  (username password error redirect)
  (setf (h:content-type*) "text/html")
  (u:log-it-pairs :debug
    :details "login-handler" 
    :username username 
    :error error
    :redirect redirect)
  (when (zerop (length redirect))
    (setf redirect nil))
  (cond
    ((and (not error) (h:session-value :jwt-token))
      (u:log-it :debug "jwt-token is present, redirecting")
      (h:redirect (or redirect "/files") :protocol :https))
    ((and (not error) username password)
      (u:log-it :debug "Login attempt for user ~a" username)
      (let ((user-id (db-user-id username password)))
        (if user-id
          (let ((token (issue-jwt user-id)))
            (u:log-it :info "Login successful for user ~a, redirecting to ~a"
              username (or redirect "/files"))
            (h:start-session)
            (setf (h:session-value :jwt-token) token)
            (h:redirect (or redirect "/files") :protocol :https))
          (progn
            (u:log-it :warn "Login failed for user ~a, render login error"
              username)
            (h:delete-session-value :jwt-token)
            (h:redirect (add-to-url-query "/login" "redirect" redirect)
              :protocol :https)))))
    (t
      (page
        (s:with-html-string
          (:div :id "login-form"
            (when error (:p (:i error)))
            (:form :id "login" :action "/login" :method "post"
              (:input :type "hidden" :name "redirect" :value redirect)
              (:input :type "text" :name "username" :placeholder "Username"
                :required t)
              (:input :type "password" :name "password" :placeholder "Password"
                :required t)
              (:button :type "submit" "Log In"))))
        :subtitle "Log In"))))

(h:define-easy-handler (logout :uri "/logout") (redirect)
  (h:delete-session-value :jwt-token)
  (h:redirect (add-to-url-query "/login" "redirect" redirect) :protocol :https))

(h:define-easy-handler (js :uri "/js") ()
  (h:handle-static-file *file-server-js*))

(defun generate-css ()
  (let ((page-background-color "#fff")
         (navbar-background-color "#eee")
         (navbar-font-family "mono")
         (nav-menu-color "#222")
         (nav-menu-after-color "#ddd")
         (nav-menu-hover-color "#0c0")
         (nav-menu-active-color "#fff"))
    (l:compile-and-write
      `(.main-page
         :background-color ,page-background-color
         :max-width "60rem"
         :margin "0 auto"
         :padding "1rem"
         (.title :font-family "mono" :font-size "2rem" :text-align "center")
         (.listing
           :list-style-type none
           (a :display "flex" :align-items "center")
           (img :margin-right "8px"))
         (.breadcrumb
           :display "flex"
           :align-items "center"
           :gap "8px"
           (img :vertical-align "middle")
           (div :display "inline" :font-size "24px"))
         (.access-list :margin-left "32px" :display "flex"
           (img :vertical-align "middle")
           (span :margin-left "4px" :font-size "14px" :margin-top "-2px"))

         (.navbar
           :background-color ,navbar-background-color
           :font-family ,navbar-font-family
           :font-size "1.2rem"
           :padding "0 2rem"
           :box-shadow "0 2px 5px rgba(0, 0, 0, 0.1)"
           (.nav-menu
             :display "flex"
             :list-style "none"
             :justify-content "center"
             (li :margin "0"
               (a
                 :display "block"
                 :color ,nav-menu-color
                 :text-decoration "none"
                 :padding "1rem 1.5rem"
                 :font-weight "500"
                 :transition "all 0.3s ease"
                 :position "relative"
                 (.user :padding-right "0.2rem")
                 (.login :font-size "0.75rem" :vertical-align "sub"))
               ("a::after"
                 :content ""
                 :position "absolute"
                 :width "0"
                 :height "3px"
                 :bottom "0"
                 :left "50%"
                 :background-color ,nav-menu-after-color
                 :transition "all 0.3s ease"
                 :transform "translateX(-50%)")
               ("a:hover"
                 :color ,nav-menu-hover-color
                 :background-color "rgba(255, 255, 255, 0.1)")
               ("a:hover::after" :width "70%")
               ("a.active"
                 :color ,nav-menu-active-color
                 :font-weight "600")
               ("a.active::after" :width "70%")
               (img :width "18px" :height "18px" :margin-right "4px"))))

         (.user-list
           :width "100%"
           :align-items "center"
           (table
             :width "100%"
             :margin-bottom "1.5rem"
             :border-spacing "0"
             (th :text-align "left"
               :border-bottom "1px solid black")
             (td :text-align "left")
             ("tr:nth-child(even)" :background-color "#f2f2f2"))
           (.pager
             :text-align "center"
             :display "flex"
             :justify-content "center"
             :align-items "center"
             :gap "0.5rem"
             :font-size "0.95rem")
           (.add-user
             :display "grid"
             :align-items "start"
             :margin-top "1rem"
             :padding "1.5rem"
             :gap "0.5rem"
             (.form-group
               :display "flex"
               :align-items "right"
               (label
                 :grid-column "1"
                 :width "20rem"
                 :text-align "right"
                 :margin-right "0.5em")
               (.textinput
                 :grid-column "2"
                 :width "50%"))
             (.checkbox-group
               :grid-column "2")
             (.button-container
               :grid-column "1 / -1"
               :justify-self "center"
               (.submit-button
                 :margin-top "1rem"))))

         (.delete-users-form 
           :width "100%"
           (.button-container
             :display "flex"
             :justify-content "right"))

         (.confirmation
           :width "100%"
           :display "flex"
           :flex-direction "column"
           (.confirmation-form
             :display "flex"
             (.button-container
               :margin-right "1rem"
               (.cancel-button
                 :color "#f00"
                 :font-size "1.1rem")
               (.confirm-button
                 :color "#0c0"
                 :font-size "1.1rem"))))

         (.bogus-class :end "end")))))

(h:define-easy-handler (css :uri "/css") ()
  (setf (h:content-type*) "text/css")
  (generate-css))

(h:define-easy-handler (favicon :uri "/favicon.ico") ()
  (h:handle-static-file *favicon*))

(h:define-easy-handler (image :uri "/image") (name)
  (h:handle-static-file (u:join-paths *web-directory* name)))

(h:define-easy-handler (root :uri "/") ()
  (h:redirect (add-to-url-query "/files" "path" "/") :protocol :https))

(h:define-easy-handler (files-handler :uri "/files") (path)
  (unless path (setf path "/"))
  (multiple-value-bind (user allowed)
    (session-user '("guest" "logged-in"))
    (let* ((abs-path (u:join-paths *document-root* path))
            (path-only (clean-path path))
            (method (h:request-method*)))
      (u:log-it-pairs :debug
        :details "files-handler"
        :user user
        :allowed allowed
        :path path
        :abs-path abs-path)

      ;; Is user authorized?
      (unless allowed
        (u:log-it-pairs :info
          :details "Authorization failed"
          :old-user user
          :new-user *guest-username*)
        (setf user *guest-username*))

      ;; Is the method GET?
      (unless (eql method :get)
        (u:log-it-pairs :warn
          :details "Method not allowed"
          :user user :method method :path path)
        (setf (h:return-code*) h:+http-method-not-allowed+)
        (return-from files-handler "Method Not Allowed"))
      (u:log-it :debug "Method is GET")

      ;; Does the file or directory exist?
      (unless (or (u:file-exists-p abs-path) (u:directory-exists-p abs-path))
        (u:log-it-pairs :warn :details "Path not found"
          :path path :abs-path abs-path :user user)
        (setf (h:return-code*) h:+http-not-found+)
        (return-from files-handler "Not Found"))
      (u:log-it :debug "File or directory exists ~a" abs-path)

      ;; Does the user have access to the path?
      (unless (has-read-access user path-only)
        (u:log-it-pairs :info
          :details "Access denied" :path path :path-only path-only :user user)
        (setf (h:return-code h:*reply*) h:+http-forbidden+)
        (return-from files-handler "Forbidden"))
      (u:log-it-pairs :info
        :details "Access granted" :user user :path path-only)

      ;; Access OK
      (if (eql (u:path-type abs-path) :directory)
        (progn
          (u:log-it :debug "~a is a directory" path)
          (render-directory-listing user path abs-path))
        (progn
          (u:log-it :debug "~a is a file" path)
          (h:handle-static-file abs-path))))))

(defun render-user-list (page page-size)
  (let ((headers (list "User" "Email" "Created" "Last Login" "Roles" ""))
         (rows (loop
                 with users = (a:list-users *rbac* page page-size)
                 for user in users
                 for username = (getf user :username)
                 for email = (getf user :email)
                 for created = (readable-timestamp (getf user :created-at))
                 for last-login = (readable-timestamp (getf user :last-login))
                 for roles = (db-list-roles username)
                 for checkbox = (input-checkbox "usernames" "" :value username
                                  :disabled 
                                  (member username '("admin" "guest" "system") 
                                    :test 'equal))
                 collect (list username email created last-login 
                           (format nil "~{~a~^, ~}" roles) checkbox))))
    (input-form "delete-users-form" "delete-users-form" "/delete-users" "post"
      (s:with-html-string
        (:raw (render-table headers rows))
        (:raw (input-submit-button "Delete Users"))))))

(defun render-table (headers rows &key (class "standard-table"))
  (unless (and (listp headers) (listp rows))
    (error "HEADERS and ROWS should be lists"))
  (when (and rows (not (listp (car rows))))
    (error "ROWS should be a list of lists"))
  (unless (every (lambda (row) (= (length row) (length headers))) rows)
    (error "Each row in ROWS must have the same length as HEADERS"))
  (let ((header-row (loop for header in headers
                      collect (s:with-html-string (:th header))
                      into header-html
                      finally (return 
                                (s:with-html-string
                                  (:tr (:raw (join-html header-html)))))))
         (rows (loop for row in rows
                 collect (loop for value in row 
                           collect (s:with-html-string 
                                     (:td (:raw (if (stringp value)
                                                  value
                                                  (format nil "~a" value)))))
                           into row-html
                           finally (return 
                                     (s:with-html-string
                                       (:tr (:raw (join-html row-html))))))
                 into rows-html
                 finally (return (join-html rows-html)))))
    (s:with-html-string
      (:table :class class
        (:thead (:raw header-row))
        (:tbody (:raw rows))))))

(defun input-hidden (name value)
  (s:with-html-string
    (:input :type "hidden" :name name :value value)))

(defun input-text (name label required &optional password)
  (s:with-html-string
    (:div :class "form-group"
      (:label :for name label)
      (:input :type (if password "password" "text")
        :id name 
        :class "textinput"
        :name name
        :required required))))

(defun input-checkbox (name display &key checked value disabled)
  (s:with-html-string
    (:div :class "checkbox"
      (:label
        (:input :type "checkbox" :name name :checked checked :value value
          :disabled disabled)
        display))))

(defun input-checkbox-list (name label values &key checked-states)
  (let ((checkboxes (loop with states = (or checked-states 
                                          (mapcar (constantly nil) values))
                      for value in values
                      for checked in states
                      for checkbox = (input-checkbox name value
                                       :checked checked :value value)
                      collect checkbox into html
                      finally (return (join-html html)))))
    (s:with-html-string
      (:div :class "form-group"
        (:label label)
        (:div :class "checkbox-group"
          (:raw checkboxes))))))

(defun input-form (id class action method &rest fields)
  (s:with-html-string
    (:form :id id :class class :action action :method method
      (:raw (join-html fields)))))

(defun input-submit-button (display &key name value (class "submit-button"))
  (s:with-html-string
    (:div :class "button-container"
      (:button :type "submit"
        :class class
        :name name
        :value value
        display))))

(defun render-new-user-form ()
  (let ((roles (a:list-role-names-regular *rbac* :page-size 1000)))
    (s:with-html-string
      (:raw (input-form "add-user" "add-user" "/add-user" "post"
              (input-text "username" "Username:" t)
              (input-text "email" "Email:" t)
              (input-text "password" "Password:" t t)
              (input-text "password-verification" "Password Verification" t t)
              (input-checkbox-list "roles" "Roles:" roles)
              (input-submit-button "Create"))))))

(defun error-page (origin user error-description &rest params)
  (let* ((err-desc (apply #'format
                     (append (list nil error-description) params)))
         (err (format nil "Error in ~a: ~a" origin err-desc)))
    (u:log-it-pairs :details err :user (or user "(unknown)"))
    (page (s:with-html-string (:p err)) :subtitle "Error" :user user)))

(h:define-easy-handler (add-user-handler :uri "/add-user"
                         :default-request-type :post)
  ((username)
    (email)
    (password)
    password-verification
    (new-roles :real-name "roles" :parameter-type '(list string)))
  (let* ((token (h:session-value :jwt-token))
          (user (when token (validate-jwt token)))
          (roles (when user (db-list-roles user))))

    (u:log-it-pairs :error :detail "handle /add-user"
      :username username
      :email email
      :passwords-match (equal password password-verification)
      :new-roles (format nil "~{~a~^, ~}" new-roles)
      :user user
      :roles (format nil "~{~a~^, ~}" roles))

    ;; Is user authorized?
    (unless (member *root-role* roles :test 'equal)
      (u:log-it-pairs :info
        :details "Authorization failed"
        :user user)
      (return-from add-user-handler
        (page
          (s:with-html-string
            (:p "Error: Authorization for /add-user failed"))
          :subtitle "Error Adding User"
          :user user)))

    ;; Do the passwords match?
    (unless (equal password password-verification)
      (return-from add-user-handler
        (error-page user "Add User" "Passwords don't match")))
    (handler-case
      ;; Add the user
      (let ((user-id (a:d-add-user *rbac* username password
                       :roles new-roles
                       :email email)))
        ;; Did the add fail?
        (unless user-id
          (return-from add-user-handler
            (error-page "Add User" user "Failed to add user '~a'" username)))
        ;; Yay! User added successfully
        (page
          (s:with-html-string
            (:p (format nil "Successfully added user ~a" username)))
          :subtitle "Added User"
          :user user))
      ;; There was a specific problem when adding the user
      (error (e)
        (error-page "ADD User" user "Failed to add user ~a. ~a" username e)))))

(h:define-easy-handler (confirm-handler :uri "/confirm")
  (source target)
  (multiple-value-bind (user allowed)
    (session-user '("admin"))
    (unless allowed
      (setf (h:return-code*) h:+http-forbidden+)
      (u:log-it-pairs :error :detail "confirm-handler"
        :status "user not allowed"
        :user user
        :source source
        :target target))
    (let ((title (h:session-value :confirmation-title))
           (description (h:session-value :confirmation-description))
           (form-action (add-to-url-query target :source source)))
      (u:log-it-pairs :debug :detail "confirm-handler"
        :status "Creating confirmation page"
        :title title
        :description description
        :form-action form-action)
      (page
        (s:with-html-string
          (:div :class "confirmation"
            (:raw (join-html 
                    (list
                      description
                      (input-form
                        "confirmation-form" 
                        "confirmation-form"
                        form-action
                        "get"
                        (input-hidden "source" source)
                        (input-hidden "target" target)
                        (input-submit-button "Cancel"
                          :class "cancel-button"
                          :name "action"
                          :value "cancel")
                        (input-submit-button "Confirm"
                          :class "confirm-button"
                          :name "action"
                          :value "confirm")))))))
        :subtitle title
        :user user))))

(defun session-user (required-roles)
  (let* ((token (h:session-value :jwt-token))
          (user (when token (validate-jwt token)))
          (roles (when user 
                   (a:list-user-role-names *rbac* user :page-size 100)))
          (union (when (some 
                         (lambda (r) (member r required-roles :test 'equal))
                         roles)
                   t))
          (allowed (when union t)))
    (u:log-it-pairs :debug :detail "session-user"
      :token token
      :required-roles (map 'vector 'identity required-roles)
      :roles (map 'vector 'identity roles)
      :allowed allowed)
    (values user allowed)))

(h:define-easy-handler (delete-users-do-handler :uri "/delete-users-do")
  (action source)
  (multiple-value-bind (user allowed)
    (session-user '("admin"))
    (u:log-it-pairs :debug :detail "delete-user-do-handler"
      :source source
      :user user
      :allowed allowed)
    (if (and user allowed (equal action "confirm"))
        (loop with users = (h:session-value :confirmation-data)
          for user in users do
          (a:d-remove-user *rbac* user)
          finally (h:redirect source :protocol :https))
      (h:redirect source :protocol :https))))

(h:define-easy-handler (delete-users-handler :uri "/delete-users"
                         :default-request-type :post)
  ((usernames :parameter-type '(list string)))
  (multiple-value-bind (user allowed)
    (session-user '("admin"))
    (u:log-it-pairs :debug 
      :detail "delete-users-handler"
      :user user
      :allowed allowed
      :usernames (map 'vector 'identity usernames))

    (unless allowed
      (setf (h:return-code*) h:+http-forbidden+)
      (return-from delete-users-handler "Forbidden"))

    (setf
      (h:session-value :confirmation-title)
      "Delete Users"
      (h:session-value :confirmation-description)
      (s:with-html-string
        (:div :class "confirmation-description"
          (:p "Please confirm that you want to delete the following users:")
          (:ul 
            (:raw (loop for username in usernames collect
                    (s:with-html-string (:li username)) into html
                    finally (return (join-html html)))))))
      (h:session-value :confirmation-data) usernames)

    (h:redirect (add-to-url-query "/confirm"
                  :source "/list-users"
                  :target "/delete-users-do")
      :protocol :https)))

(defun render-pager (url current-page page-size element-count
                      &optional (link-count 5))
  (u:log-it-pairs :debug :detail "render-pager"
    :url url
    :current-page current-page
    :page-size page-size
    :element-count element-count
    :link-count link-count)
  (loop
    with page-count = (ceiling element-count page-size)
    with link-count-half = (floor link-count 2)
    with first-page = (max 1 (- current-page link-count-half))
    with last-page = (min page-count (+ current-page link-count-half))
    with page-1 = (when (> first-page 1) 1)
    with page-n = (when (< last-page page-count) page-count)
    with check-current-page = (when (or
                                      (< current-page 1)
                                      (> current-page page-count))
                                (error
                                  (u:log-it-pairs :error
                                    :details "CURRENT-PAGE is out of bounds"
                                    :current-page curent-page
                                    :first-page 1
                                    :last-page page-count)))
    with pages = (remove-if-not
                   #'identity
                   (append
                     (list page-1)
                     (loop for page from first-page to last-page collect page)
                     (list page-n)))
    for page in pages
    for next-page in (cdr (append pages (list 0)))
    for index from 1 to (length pages)
    collect (s:with-html-string
              (:a :class (if (= page current-page) "current-page" "page")
                :href (add-to-url-query url "page" page)
                (format nil "~d" page))
              (:span :class "page-separator"
                (if (> next-page (1+ page)) "..." " ")))
    into pager
    finally (return
              (if (> (length pages) 1)
                (s:with-html-string
                  (:comment "Pager")
                  (:div :class "pager"
                    (:span :class "title" "Page: ")
                    (:span :class "pages" (:raw (format nil "~{~a~}" pager)))))
                ""))))

(h:define-easy-handler (list-users-handler :uri "/list-users")
  ((page :parameter-type 'integer :init-form 1)
    (page-size :parameter-type 'integer :init-form 20))
  (let* ((method (h:request-method*))
          (token (h:session-value :jwt-token))
          (user (when token (validate-jwt token))))

    (u:log-it-pairs :debug :details "Handling /list-users"
      :token token :user user :method method
      :page page :page-size page-size)

    ;; Is user authorized?
    (unless (equal user *root-username*)
      (u:log-it-pairs :info
        :details "Authorization failed"
        :user user
        :error (format nil "Only ~a user is allowed" *root-username*)
        (h:redirect "/files" :protocol :https)))

    ;; Is the method GET?
    (unless (eql method :get)
      (u:log-it-pairs :warn
        :details "Method not allowed"
        :user user :method method)
      (setf (h:return-code*) h:+http-method-not-allowed+)
      (return-from list-users-handler "Method Not Allowed"))

    (page
      (s:with-html-string
        (:div :class "user-list"
          (:raw (render-user-list page page-size))
          (:raw (render-pager "/list-users"
                  page page-size (a:list-users-count *rbac*)))
          (:raw (render-new-user-form))))
      :user user
      :subtitle "List Users")))

(defun start-web-server ()
  (setf *http-server* (make-instance 'fs-acceptor
                        :port *http-port*
                        :document-root *document-root*))
  (setf
    h:*show-lisp-errors-p* t
    (h:acceptor-persistent-connections-p *http-server*) nil)
  (u:log-it :info "Server started on http://localhost:~d" *http-port*)
  (h:start *http-server*))

(defun stop-web-server ()
  (h:stop *http-server*)
  (setf *http-server* nil))

(defun init-database ()
  (u:log-it :info
    "host=~a; port=~a; db=~a; user=~a; password=~a"
    *db-host* *db-port* *db-name* *db-username* *db-password*)
  (setf *rbac* (make-instance 'a:rbac-pg
                 :host *db-host*
                 :port *db-port*
                 :dbname *db-name*
                 :username *db-username*
                 :password *db-password*
                 :resource-regex "^/([-_.a-zA-Z0-9 ]+/)*$"))
  ;; Add root role if it doesn't exist
  (unless (a:get-id *rbac* "roles" *root-role*)
    (a:d-add-role *rbac* *root-role*
      :description "The administrative role."))
  ;; Add root user if it doesn't exist
  (unless (a:get-id *rbac* "users" *root-username*)
    (a:d-add-user *rbac* *root-username* *root-password*
      :roles (list *root-role*)))
  ;; Add guest role if it doesnt exist
  (unless (a:get-id *rbac* "roles" *guest-role*)
    (a:d-add-role *rbac* *guest-role*
      :description "Role for anonymous users that are not logged in.")
    (a:d-add-role-permission *rbac* *guest-role* "read"))
  ;; Add the guest user if it doesn't exist
  (unless (a:get-id *rbac* "users" *guest-username*)
    (a:d-add-user *rbac* *guest-username* *guest-password*
      :roles (list *guest-role*)))
  ;; Remove logged-in role from guest if necessary
  (when (member "logged-in" (db-list-roles *guest-username*)
          :test 'equal)
    (a:d-remove-user-role *rbac* *guest-username* "logged-in"))
  ;; Add the root directory to the resources, in such a way that
  ;; everyone (guest) have access
  (unless (a:get-id *rbac* "resources" "/")
    (a:d-add-resource *rbac* "/" :roles (list *guest-role*)))
  *rbac*)

(defun periodic-directory-sync ()
  ;; Continuously sync the file system directories with the rbac resources
  ;; (directories, as tracked in the rbac system). If a new directory appears in
  ;; the file system, it should be added to the rbac system.  If a directory goes
  ;; missing from the file system, it should be removed from the rbac system.
  (loop while *directory-syncing*
    for old-hash = "" then new-hash
    for new-hash = (hash-directory-list (fs-list-directories))
    unless (equal old-hash new-hash)
    do (sync-directories)
    do (sleep 5)))

(defun run ()
  (u:open-log *log-file* :severity-threshold *log-severity-threshold*)
  (u:log-it :info "Initializing database")
  ;; Initialize the database
  (let ((success (handler-case (init-database)
                   (error (condition)
                     (u:log-it :error (format nil "~a" condition))
                     nil))))
    (u:log-it :debug "Database initialization: ~a"
      (if success "success" "failure"))

    ;; Start the Swank server
    (unless *swank-server*
      (u:log-it :info "Starting Swank")
      (setf *swank-server*
        (swank:create-server
          :interface "0.0.0.0"
          :port 4005
          :style :spawn
          :dont-close t)))
    (when (and success (not *http-server*))
      ;; Start the Web server
      (start-web-server))
    (loop while t do
      (periodic-directory-sync)
      (sleep 5))))
