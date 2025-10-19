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
    (mapcar (lambda (dir) (subseq dir l)) dirs)))

(defun sync-directories ()
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
      (u:log-it :info "added directorie~p: ~{~a~^, ~}" 
        (length added) added))
    (when removed
      (u:log-it :info "removed directorie~p: ~{~a~^, ~}"
        (length removed) removed))))
                     
(defun clean-path (path)
  (if (equal path "/")
    path
    (let* ((path-only (u:path-only path))
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
                             (format nil "<a href=\"?path=~a\">~a</a>"
                               parent-path parent-name))
                   into breadcrumbs
                   finally (return (format nil "~{~a~^/~}" breadcrumbs)))))
    (u:log-it :info "List directories")
    (u:log-it :debug "Files: ~{~a~^, ~}" files)
    (u:log-it :debug "Subdirs: ~{~a~^, ~}" subdirs)
    (format nil "<h1>Donnie's Bad-Ass File Server</h1>
<h2>~a</h2>
<ul>
~{~a~^~%~}~{~a~^~%~}~%
</ul>"
      crumbs
      (mapcar
        (lambda (d)
          (format nil "<li><a href=\"/files?path=~a\">~a</a></li>" 
            d 
            (u:leaf-directory-only d)))
        subdirs)
      (mapcar 
        (lambda (f)
          (format nil "<li><a href=\"/files?path=~a\">~a</a></li>" 
            f 
            (u:filename-only f)))
        files))))

(defun get-current-datetime ()
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month day hour minute second)))

(h:define-easy-handler (health :uri "/health") ()
  (format nil "<html><body><h1>OK</h1>~a</body></html>~%"
    (get-current-datetime)))

(h:define-easy-handler (root :uri "/") ()
  (h:redirect "/files?path=/"))

(h:define-easy-handler (main-handler :uri "/files") (path)
  (u:log-it :debug "Processing request ~a" h:*request*)
  (u:log-it :debug "Authorization header: ~a" (h:header-in "authorization" h:*request*))
  (unless path (setf path "/"))
  (multiple-value-bind (user password) (h:authorization)
    (unless user
      (return-from main-handler (h:require-authorization "File Server")))
    (let* ((abs-path (u:join-paths *document-root* path))
            (user-id (db-user-id user password))
            (method (h:request-method h:*request*)))
      (u:log-it :debug "User=~a; UserID=~a; Password=~a; Method=~a; Path=~a; AbsPath=~a" 
        user user-id password method path abs-path)
      ;; Is user authorized?
      (unless user-id
        (return-from main-handler (h:require-authorization "File Server")))
      (u:log-it :info "User is authorized")
      ;; Is the method GET?
      (unless (eql method :get)
        (u:log-it :error (format nil "~a; username=~a; method=~a; path=~a;"
                           "Method not allowed" user method path))
        (setf (h:return-code h:*reply*) 405)
        (return-from main-handler "Method Not Allowed"))
      (u:log-it :debug "Method is GET")
      ;; Does the file or directory exist?
      (unless (or (u:file-exists-p abs-path) (u:directory-exists-p abs-path))
        (u:log-it :error (format nil "~a; username=~a; method=~a; path=~a;"
                           "File not found" user method path))
        (setf (h:return-code h:*reply*) 404)
        (return-from main-handler "Not Found"))
      (u:log-it :debug "File or directory exists ~a" abs-path)
      ;; Does the user have access to the path?
      (unless (has-read-access user path)
        (u:log-it :info "~a; method=~a; path=~a; user=~a; password=~a;"
          "Access denied" method path user password)
        (setf (h:return-code h:*reply*) 403)
        (return-from main-handler "Forbidden"))
      (u:log-it :info "User ~a has access to directory or file ~a"
        user path)
      ;; Access OK
      (if (eql (u:path-type abs-path) :directory)
        (progn
          (u:log-it :debug "~a is a directory" path)
          (render-directory-listing user path abs-path))
        (progn
          (u:log-it :debug "~a is a file" path)
          (h:handle-static-file abs-path))))))

(defun start-web-server ()
  (setf *http-server* (make-instance 'h:easy-acceptor
                        :port *http-port*
                        :document-root *document-root*))
  (setf
    h:*show-lisp-errors-p* t
    (h:acceptor-persistent-connections-p *http-server*) nil)
  (u:log-it :info "Server started on http://localhost:~d" *http-port*)
  (h:start *http-server*))

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
      (start-web-server)))

  ;; Continuously sync the file system directories with the rbac resources
  ;; (directories, as tracked in the rbac system). If a new directory appears in
  ;; the file system, it should be added to the rbac system.  If a directory goes
  ;; missing from the file system, it should be removed from the rbac system.
  (loop while *directory-syncing* do (sync-directories) (sleep 5)))
