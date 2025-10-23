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
                                :default "admin-password"))
(defparameter *root-role* "admin")

;; JWT Secret
(defparameter *jwt-secret* (b:string-to-octets
                             (u:getenv "JWT_SECRET" :required t)))

;; Javascript
(defparameter *js-directory* (u:getenv "FRONT_END_DIRECTORY"
                               :default "/app/js"))
(defparameter *file-server-js* (u:join-paths *js-directory* "file-server.js"))
(defparameter *favicon* "/app/favicon.ico")

;; HTTP and Swank servers
(defparameter *http-port* (u:getenv "HTTP_PORT" :default 8080 :type :integer))
(defparameter *document-root* (u:getenv "FS_DOCUMENT_ROOT"
                                :default "/app/shared-files/"))
(defparameter *swank-port* (u:getenv "SWANK_PORT" :default 4005 :type :integer))

;; Logs
(defparameter *log-file* (or (u:getenv "LOG_FILE") *standard-output*))
(defparameter *log-severity-threshold*
  (intern (string-upcase (or (u:getenv "LOG_SEVERITY") "DEBUG")) :keyword))

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
  (let ((log-severity (cond
                        ((equal (h:request-uri*) "/health") :debug)
                        ((< (h:return-code*) 300) :info)
                        ((< (h:return-code*) 500) :warn)
                        (t :error))))
    (u:log-it-pairs log-severity
      :type "access"
      :remote (h:remote-addr*)
      :server (h:local-addr*)
      :host (h:host)
      :method (h:request-method*)
      :uri (h:request-uri*)
      :return-code (h:return-code*)
      :status return-code
      :content-length (or (h:content-length*) 0)
      :referer (h:referer)
      :agent (h:user-agent))))

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

(defun db-directory-id (directory)
  "Determines if DIRECTORY exists as a resource in the database, returning the
directory's ID if it does and NIL otherwise."
  (a:get-id *rbac* "resources" directory))

(defun db-user-id (user password)
  (a:d-login *rbac* user password))

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

(defun page (content &key subtitle user)
  (let ((title "Donnie's Bad-Ass File Server"))
    (s:with-html-string
      (:doctype)
      (:html
        (:head
          (:title title))
        (:body
          (when user
            (:p :class "user"
              user
              (:form
                :id "logout-form"
                :action "/logout"
                :method "get"
                (:button :type "submit" "Logout"))))
          (:h1 title)
          (when subtitle (:h2 subtitle))
          (:raw content))))))

(defun render-directory-listing (user path abs-path)
  (setf (h:content-type*) "text/html")
  (let ((files (list-files abs-path))
         (subdirs (rdl-subdirectories user abs-path))
         (crumbs (loop
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
                   finally (return (format nil "~{~a~^/~}" breadcrumbs)))))
    (u:log-it :info "List directories")
    (u:log-it :debug "Files: ~{~a~^, ~}" files)
    (u:log-it :debug "Subdirs: ~{~a~^, ~}" subdirs)
    (page (s:with-html-string
            (:h2 (:raw crumbs))
            (mapcar
              (lambda (d)
                (:li (:a :href (format nil "/files?path=~a" d)
                       (u:leaf-directory-only d))))
              subdirs)
            (mapcar
              (lambda (f)
                (:li (:a :href (format nil "/files?path=~a" f)
                       (u:filename-only f))))
              files))
      :user user)))

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

(h:define-easy-handler (login :uri "/login") (username password error)
  (setf (h:content-type*) "text/html")
  (u:log-it-pairs :debug :details "Login page" :username username :error error)
  (cond
    ((and (not error) (h:session-value :jwt-token))
      (u:log-it :debug "jwt-token is present, redirecting to files")
      (h:redirect "/files?path=/"))
    ((and (not error) username password)
      (u:log-it :debug "Login attempt for user ~a" username)
      (let ((user-id (db-user-id username password)))
        (if user-id
          (let ((token (issue-jwt user-id)))
            (u:log-it :info "Login successful for user ~a, redirecting"
              username)
            (h:start-session)
            (setf (h:session-value :jwt-token) token)
            (h:redirect "/files?path=/"))
          (progn
            (u:log-it :warn "Login failed for user ~a, render login error"
              username)
            (h:delete-session-value :jwt-token)
            (h:redirect "/login?error=Invalid+username+or+password")))))
    (t
      (u:log-it :debug "Rendering login page")
        (page
          (s:with-html-string
            (:div :id "login-form"
              (when error (:p error))
              (:form :id "login" :action "/login" :method "post"
                (:input :type "text" :name "username" :placeholder "Username"
                  :required t)
                (:input :type "password" :name "password" :placeholder "Password"
                  :required t)
                (:button :type "submit" "Login"))))
          :subtitle "Login"))))

(h:define-easy-handler (logout :uri "/logout") ()
  (h:delete-session-value :jwt-token)
  (h:redirect "/login"))

(h:define-easy-handler (js :uri "/js") ()
  (h:handle-static-file *file-server-js*))

(h:define-easy-handler (favicon :uri "/favicon.ico") ()
  (h:handle-static-file *favicon*))

(h:define-easy-handler (root :uri "/") ()
  (h:redirect "/files?path=/"))

(h:define-easy-handler (files-handler :uri "/files") (path)
  (unless path (setf path "/"))
  (let* ((abs-path (u:join-paths *document-root* path))
          (path-only (clean-path path))
          (method (h:request-method*))
          (token (h:session-value :jwt-token))
          (user (when token (validate-jwt token))))

    (u:log-it-pairs :debug
      :details "Handling /files request"
      :token token :user user :method method :path path :abs-path abs-path)

    ;; Is user authorized?
    (unless user
      (u:log-it-pairs :warn :details "Authorization failed" :user user)
      (setf (h:return-code*) h:+http-authorization-required+)
      (h:redirect "/login"))
    (u:log-it :info "User is authorized")

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
        (h:handle-static-file abs-path)))))

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
