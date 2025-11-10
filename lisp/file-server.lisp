(in-package :file-server)

;; Database
(defparameter *db-host* (u:getenv "DB_HOST" :default "localhost"))
(defparameter *db-port* (u:getenv "DB_PORT" :default 5432 :type :integer))
(defparameter *db-name* (u:getenv "DB_NAME" :default "fileserver"))
(defparameter *db-username* (u:getenv "DB_USER" :default "fileserver"))
(defparameter *db-password* (u:getenv "DB_PASSWORD"
                              :default "fileserver-password"))

;; Users
(defparameter *admin* "admin")
(defparameter *admin-password* (u:getenv "ADMIN_PASSWORD"
                                 :default "admin-password-1234"))
(defparameter *system* "system")
(defparameter *guest* "guest")
(defparameter *guest-password* "guest-password-1234")

;; Roles
(defparameter *admin-role* "admin")
(defparameter *system-role* "system")
(defparameter *public-role* "public")
(defparameter *logged-in-role* "logged-in")

;; Tables
(defparameter *users-table* "users")
(defparameter *roles-table* "roles")
(defparameter *resources-table* "resources")

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

;; Version
(defparameter *version* (u:getenv "FILE_MANAGER_VERSION" :default "0.0"))

;; Other
(defparameter *http-server* nil)
(defparameter *swank-server* nil)
(defparameter *root-userid* nil)
(defparameter *rbac* nil)
(defparameter *directory-syncing* t)
(defparameter *max-page-size* 1000)

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

(defun db-directory-id (directory)
  "Determines if DIRECTORY exists as a resource in the database, returning the
directory's ID if it does and NIL otherwise."
  (a:get-id *rbac* *resources-table* directory))

(defun db-user-id (user password)
  (a:d-login *rbac* user password))

(defun db-list-roles (user &optional all-roles)
  (when user
    (handler-case
      (if all-roles
        (user-roles user)
        (regular-user-roles user))
      (error (e)
        (u:log-it-pairs :error :in "db-list-roles"
          :status "failed to retrieve roles for user"
          :user user
          :error (format nil "~a" e))
        nil))))

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
  (u:log-it-pairs :debug :in "sync-directories")
  (let* ((fs-dirs (fs-list-directories))
          (db-dirs (resouces))
          (added (loop
                   for dir in fs-dirs
                   unless (db-directory-id dir)
                   do (a:d-add-resource *rbac* dir :roles (list *admin-role*))
                   and collect dir))
          (removed (loop for dir in db-dirs
                     unless (has fs-dirs dir)
                     do (a:d-remove-resource *rbac* dir)
                     and collect dir)))
    (when added
      (u:log-it-pairs :info :in "sync-directories"
        :status "added directories"
        :directories added))
    (when removed
      (u:log-it-pairs :info :in "sync-directories"
        :status "removed directories"
        :directories removed))))

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
      (u:log-it-pairs :debug :in "clean-path"
        :path-only path
        :clean-path clean-path)
      clean-path)))

(defun has-read-access (user path)
  (u:log-it-pairs :debug :in "has-read-access"
    :status "checking access" :user user :permission "read" :path path)
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

(defun menu (user subtitle)
  (let* ((roles (db-list-roles user t))
          (is-admin (has roles *admin-role*)))
    (s:with-html-string
      (:nav :class "navbar"
        (:ul :class "nav-menu"
          (:li (:a :href "/files" "files"))
          (when is-admin
            (list
              (:li (:a :href "/list-users" "list users"))
              (:li (:a :href "/list-roles" "list roles"))))
          (if user
            (:li (:a :href "/logout"
                   (:img :src "/image?name=user.png")
                   (:span :class "user" user)
                   (:span :class "login"
                     (if (equal user *guest*)
                       "[log in]"
                       "[log out]"))))
            (unless (equal subtitle "Log In")
              (:li (:a :href "/logout" "log in")))))))))

(defun page (content &key subtitle user)
  (u:log-it-pairs :debug :in "page"
    :status "rendering page" :subtitle subtitle :user user)
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
            (:raw content)
            (:div :class "status-line"
              (:span "Version:" *version*))))))))

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

(defun directory-roles (path)
  (let* ((count-listed 5)
          (count-total 20)
          (raw-roles (resource-roles path))
          (admin-roles (list *admin-role* "system"))
          (roles (if (has raw-roles *public-role*)
                   (list *public-role*)
                   (exclude raw-roles admin-roles)))
          (count-actual (length roles))
          (roles-show (if (<= (length roles) count-listed)
                        roles
                        (subseq roles 0 count-listed)))
          (additional (additional-text count-actual count-listed count-total)))
    (when additional
      (setf roles-show
        (append roles-show (list additional))))
    roles-show))

(defun render-directory-listing (user path abs-path)
  (setf (h:content-type*) "text/html")
  (let ((files (list-files abs-path))
         (subdirs (rdl-subdirectories user abs-path))
         (crumbs (assemble-breadcrumbs path))
         (roles (directory-roles path)))
    (u:log-it-pairs :debug :in "render-directory-listing"
      :status "list directories"
      :files files
      :subdirectories subdirs
      :access-list roles)
    (page (s:with-html-string
            (:div :class "breadcrumb"
              (:img :src "/image?name=home.png" :width 24 :height 24)
              (:div (:raw crumbs)))
            (:div :class "access-list"
              (:img :src "/image?name=users.png" :width 16 :height 16)
              (:span (format nil "~{~a~^, ~}" roles)))
            ;; Directories
            (:ul :class "listing"
              (loop for dir in subdirs
                for name = (u:leaf-directory-only dir)
                for dir-roles = (format nil "~{~a~^, ~}" (directory-roles dir))
                for href = (format nil "/files?path=~a" dir)
                for image = "/image?name=folder.png"
                collect
                (:li
                  (:a :href href (:img :src image :width 24 :height 24) name)
                  (:span dir-roles))))
            ;; Files
            (:ul :class "listing"
              (mapcar
                (lambda (f)
                  (:li (:a :href (format nil "/files?path=~a" f)
                         :target "_blank"
                         (:img :src "/image?name=file.png"
                           :width 24 :height 24)
                         (u:filename-only f))))
                files))
            (:raw (render-new-directory-form user path)))
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
  (u:log-it-pairs :debug :in "login-handler"
    :username username
    :error error
    :redirect redirect)
  (when (zerop (length redirect))
    (setf redirect nil))
  (cond
    ((and (not error) (h:session-value :jwt-token))
      (u:log-it-pairs :debug :in "login-handler"
        :status "jwt-token is present, redirecting")
      (h:redirect (or redirect "/files") :protocol :https))
    ((and (not error) username password)
      (u:log-it-pairs :debug :in "login-handler"
        :status "login attempt"
        :user username)
      (let ((user-id (db-user-id username password)))
        (if user-id
          (let ((token (issue-jwt user-id)))
            (u:log-it-pairs :info :in "login-handler"
              :status "login successful"
              :user username
              :redirect (or redirect "/files"))
            (h:start-session)
            (setf (h:session-value :jwt-token) token)
            (h:redirect (or redirect "/files") :protocol :https))
          (progn
            (u:log-it-pairs :warn :in "login-handler"
              :status "login failed"
              :username username
              :redirect (add-to-url-query "/login" "redirect" redirect))
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
  (multiple-value-bind (user allowed required-roles)
    (session-user (list *public-role* *logged-in-role*))
    (let* ((abs-path (u:join-paths *document-root* path))
            (path-only (clean-path path))
            (method (h:request-method*)))
      (u:log-it-pairs :debug :in "files-handler"
        :user user
        :allowed allowed
        :required-roles required-roles
        :path path
        :abs-path abs-path)

      ;; Is user authorized?
      (unless allowed
        (u:log-it-pairs :info :in "file-handler"
          :status "authorization failed"
          :old-user user
          :new-user *guest*)
        (setf user *guest*))

      ;; Is the method GET?
      (unless (eql method :get)
        (u:log-it-pairs :warn :in "file-handler"
          :status "method not allowed"
          :user user :method method :path path)
        (setf (h:return-code*) h:+http-method-not-allowed+)
        (return-from files-handler "Method Not Allowed"))
      (u:log-it-pairs :debug :in "files-handler"
        :status "Method is GET")

      ;; Does the file or directory exist?
      (unless (or (u:file-exists-p abs-path) (u:directory-exists-p abs-path))
        (u:log-it-pairs :warn :in "files-handler"
          :status "path not found" :path path :abs-path abs-path :user user)
        (setf (h:return-code*) h:+http-not-found+)
        (return-from files-handler "Not Found"))
      (u:log-it-pairs :debug :in "file-handler"
        :status "file or directory exists"
        :file-or-directory abs-path)

      ;; Does the user have access to the path?
      (unless (has-read-access user path-only)
        (u:log-it-pairs :info :in "file-handler"
          :status "access denied"
          :path path :path-only path-only :user user)
        (setf (h:return-code h:*reply*) h:+http-forbidden+)
        (return-from files-handler "Forbidden"))
      (u:log-it-pairs :info :in "file-handler"
        :status "access granted" :user user :path path-only)

      ;; Access OK
      (if (eql (u:path-type abs-path) :directory)
        (progn
          (u:log-it-pairs :debug :in "file-handler"
            :status "path is a directory"
            :path path)
          (render-directory-listing user path abs-path))
        (progn
          (u:log-it-pairs :debug :in "file-handler"
            :status "path is a file"
            :path path)
          (h:handle-static-file abs-path))))))

(defun render-user-list (page page-size)
  (let ((headers (list "User" "Email" "Created" "Last Login" "Roles" ""))
         (rows (loop
                 with users = (exclude (a:list-users *rbac* page page-size)
                                (list *system*))
                 and fixed-users = (list *admin* *guest*)
                 for user in users
                 for username = (getf user :username)
                 for email = (getf user :email)
                 for created = (readable-timestamp (getf user :created-at))
                 for last-login = (readable-timestamp (getf user :last-login))
                 for roles = (db-list-roles username)
                 for checkbox = (input-checkbox "usernames" "" :value username
                                  :disabled (has fixed-users username))
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

(defun permission-names ()
  (a:list-permission-names *rbac* :page-size *max-page-size*))

(defun role-names ()
  (a:list-role-names *rbac* :page-size *max-page-size*))

(defun regular-roles ()
  (a:list-role-names-regular *rbac* :page-size *max-page-size*))

(defun user-roles (user)
  (a:list-user-role-names *rbac* user :page-size *max-page-size*))

(defun regular-user-roles (user)
  (a:list-user-role-names-regular *rbac* user :page-size *max-page-size*))

(defun resource-names ()
  (a:list-resource-names *rbac* :page-size *max-page-size*))

(defun resource-roles (resource)
  (a:list-resource-role-names *rbac* resource :page-size *max-page-size*))

(defun regular-resource-roles (resource)
  (a:list-resource-role-names-regular *rbac* resource
    :page-size *max-page-size*))

(defun render-new-user-form ()
  (let ((roles (regular-roles)))
    (input-form "add-user" "add-user" "/add-user" "post"
      (input-text "username" "Username:" t)
      (input-text "email" "Email:" t)
      (input-text "password" "Password:" t t)
      (input-text "password-verification" "Password Verification" t t)
      (input-checkbox-list "roles" "Roles:" roles)
      (input-submit-button "Create"))))

(defun role-options (user parent)
  (let ((user-roles (user-roles user))
         (parent-roles (resource-roles parent))
         (exceptions (list (exclusive-role-for-user user))))
    (if (has parent-roles "public")
      (exclude-except user-roles ":exclusive$" exceptions)
      (exclude-except
        (intersection parent-roles user-roles)
        exceptions))))

(defun render-new-directory-form (user parent)
  (input-form "add-directory" "add-directory" "/add-directory" "post"
    (input-text "directory" "Directory Name (no slashes):" t)
    (input-checkbox-list "roles" "Roles: " (role-options user parent))
    (input-hidden "user" user)
    (input-hidden "parent" parent)
    (input-submit-button "Create")))

(defun error-page (action user error-description &rest params)
  (let* ((err-desc (apply #'format
                     (append (list nil error-description) params)))
         (err (format nil "Error while ~a: ~a" action err-desc)))
    (u:log-it-pairs :error :in "error-page"
      :status err :user (or user "(unknown)"))
    (page (s:with-html-string (:p err)) :subtitle "Error" :user user)))

(defun success-page (user description &rest params)
  (let* ((desc (apply #'format
                 (append (list nil description) params))))
    (u:log-it-pairs :debug :in "success-page"
      :user user
      :description desc)
    (page (s:with-html-string (:p desc)) :subtitle "Success" :user user)))

(defmacro define-add-handler
  ;; *admin-role* is not available during compilation, we we're using
  ;; its value directly here.
  ((handler-name uri &key (required-roles (list "admin")))
    http-parameters
    (&rest validation-clauses)
    add-function
    element-name)
  `(h:define-easy-handler (,handler-name :uri ,uri :default-request-type :post)
     ,http-parameters
     (multiple-value-bind (user allowed required-roles)
       (session-user ',required-roles)

       (let* ((param-specs ',http-parameters)
               (name-sym (cond
                           ((null param-specs)
                             (error "http-parameters empty"))
                           ((listp (first param-specs))
                             (first (first param-specs)))
                           (t (first param-specs))))
               (name-param (h:parameter
                             (string-downcase (symbol-name name-sym))))
               (action (format nil "adding ~a '~a'" ,element-name name-param))
               (handler (format nil "~(~a~)" ',handler-name))
               (log-pairs (append
                            (list
                              :debug
                              :in handler
                              :user user
                              :allowed allowed
                              :required-roles required-roles)
                            (list ,@(loop
                                      for spec in http-parameters
                                      for var = (if (listp spec) (first spec) spec)
                                      for kw = (intern (string-upcase var) "KEYWORD")
                                      collect kw
                                      collect var)))))

         ;; Log the request
         (apply #'u:log-it-pairs log-pairs)

         ;; Authorization
         (unless allowed
           (u:log-it-pairs :error :in handler
             :status "Authorization failed"
             :user user
             :allowed allowed
             :required-roles required-roles)
           (return-from ,handler-name
             (error-page action user "Authorization failed")))

         ;; Validation
         ,@(loop for (test msg) in validation-clauses
             collect `(unless ,test
                        (return-from ,handler-name
                          (error-page action user ,msg))))

         ;; Add the element
         (handler-case
           (let ((id (handler-case
                       ,add-function
                       (error (e)
                         (u:log-it-pairs :error :in handler
                           :status (format nil "~a" e))
                         (return-from ,handler-name
                           (error-page action user
                             (format nil "~a" e)))))))
             (unless id
               (u:log-it-pairs :error :in handler
                 :status (format nil "Failed to add ~a '~a'"
                           ,element-name name-param))
               (return-from ,handler-name
                 (error-page action user
                   "Failed to add ~a '~a'" ,element-name name-param)))
             (success-page user "Success ~a." action))
           (error (e)
             (error-page action user e)))))))

(define-add-handler (add-user-handler "/add-user")
  (username email password password-verification
    (new-roles :real-name "roles" :parameter-type '(list string)))
  (((equal password password-verification) "Passwords don't match"))
  (a:d-add-user *rbac* username password
    :roles new-roles
    :email email)
  "user")

(define-add-handler (add-role-handler "/add-role")
  (role description (permissions :parameter-type '(list string)))
  nil
  (a:d-add-role *rbac* role :description description :permissions permissions)
  "role")

(define-add-handler (add-directory-handler "/add-directory"
                      :required-roles (list *logged-in-role*))
  (directory parent user
    (new-roles :real-name "roles" :parameter-type '(list string)))
  (((not (re:scan "/" directory)) "Directory has slashes")
    ((or (equal parent "/") (re:scan "^/.+/$" parent))
      "Parent directory must be absolute and end in a slash")
    ((u:directory-exists-p (u:join-paths *document-root* parent))
       "Parent directory not in file system")
    ((not (has (resources) (concatenate 'string parent directory "/")))
      "Directory already exists")
    ((a:get-id *rbac* *users-table* user)
      (format nil "User '~a' doesn't exist" user))
    ((loop for role in new-roles always (a:get-id *rbac* *roles-table* role))
      "One or more roles doesn't exist"))
  (let* ((resource (concatenate 'string parent directory "/"))
          (absolute-path (concatenate 'string
                           (u:join-paths *document-root* resource) "/"))
          (all-roles (cons (format nil "~a:exclusive" user) new-roles)))
    (u:log-it-pairs :debug :in "add-directory-handler"
      :resource resource
      :absolute-path absolute-path
      :all-roles all-roles)
    (ensure-directories-exist absolute-path)
    (a:d-add-resource *rbac* resource :roles all-roles))
  "directory")

(h:define-easy-handler (confirm-handler :uri "/confirm")
  (source target)
  (multiple-value-bind (user allowed required-roles)
    (session-user (list *logged-in-role*))
    (u:log-it-pairs :debug :in "confirm-handler"
      :source source
      :target target
      :user user
      :allowed allowed
      :required-roles required-roles)
    (unless allowed
      (u:log-it-pairs :error :in "confirm-handler"
        :status "Not authorized"
        :user user
        :allowed allowed
        :required-roles required-roles
        :source source
        :target target)
      (setf (h:return-code*) h:+http-forbidden+)
      (return-from confirm-handler "Forbidden"))
    (let ((title (h:session-value :confirmation-title))
           (description (h:session-value :confirmation-description))
           (form-action (add-to-url-query target :source source)))
      (u:log-it-pairs :debug :in "confirm-handler"
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
          (user-roles (when user (user-roles user)))
          (allowed (has-some required-roles user-roles)))
    (u:log-it-pairs :debug :in "session-user"
      :token token
      :required-roles required-roles
      :roles user-roles
      :allowed allowed)
    (values user allowed required-roles)))

(defmacro define-list-handler
  ;; *admin-role* is not available during compilation, we we're using
  ;; its value directly here.
  ((handler-name uri &key (required-roles (list "admin")))
    http-parameters
    render-list-function
    list-count-function
    render-new-form-function
    list-name)
  `(h:define-easy-handler (,handler-name :uri ,uri :default-request-type :get)
     ,http-parameters
     (multiple-value-bind (user allowed required-roles)
       (session-user ',required-roles)

       (let* ((action (format nil "listing ~a" ',list-name))
               (handler (format nil "~(~a~)" ',handler-name))
               (log-pairs (append
                            (list
                              :debug
                              :in handler
                              :user user
                              :allowed allowed
                              :required-roles required-roles)
                            (list ,@(loop
                                      for spec in http-parameters
                                      for var = (if (listp spec) (first spec) spec)
                                      for kw = (intern (string-upcase var) "KEYWORD")
                                      collect kw
                                      collect var)))))

         ;; Log the request
         (apply #'u:log-it-pairs log-pairs)

         ;; Authorization
         (unless allowed
           (u:log-it-pairs :error :in handler
             :status "Authorization failed"
             :user user
             :allowed allowed
             :required-roles required-roles)
           (return-from ,handler-name
             (error-page action user "Authorization failed")))

         ;; Is the method GET?
         (unless (eql (h:request-method*) :get)
           (u:log-it-pairs :warn :in handler
             :status "HTTP method not supported"
             :user user
             :method (h:request-method*))
           (return-from ,handler-name
             (error-page action user "Method not allowed")))

         ;; Assemble page
         (page
           (s:with-html-string
             (:div :class (format nil "~a-list" ,list-name)
               (:raw ,render-list-function)
               (:raw (render-pager
                       (car (re:split "\\?" (h:request-uri*)))
                       page page-size ,list-count-function))
               (:raw ,render-new-form-function)))
           :user user
           :subtitle (format nil "List ~@(~a~)" ,list-name))))))

(define-list-handler (list-roles-handler "/list-roles")
  ((page :parameter-type 'integer :init-form 1)
    (page-size :parameter-type 'integer :init-form 20))
  (render-role-list page page-size)
  (a:list-roles-regular-count *rbac*)
  (render-new-role-form)
  "roles")

(define-list-handler (list-users-handler "/list-users")
  ((page :parameter-type 'integer :init-form 1)
    (page-size :parameter-type 'integer :init-form 20))
  (render-user-list page page-size)
  (a:list-users-count *rbac*)
  (render-new-user-form)
  "users")

(h:define-easy-handler (delete-users-handler :uri "/delete-users"
                         :default-request-type :post)
  ((usernames :parameter-type '(list string)))
  (multiple-value-bind (user allowed required-roles)
    (session-user (list *admin-role*))
    (u:log-it-pairs :debug :in "delete-users-handler"
      :user user
      :allowed allowed
      :required-roles required-roles
      :users-to-delete usernames)

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

(h:define-easy-handler (delete-users-do-handler :uri "/delete-users-do")
  (action source)
  (multiple-value-bind (user allowed required-roles)
    (session-user (list *admin-role*))
    (u:log-it-pairs :debug :in "delete-user-do-handler"
      :source source
      :user user
      :allowed allowed
      :required-roles required-roles)
    (if (and user allowed (equal action "confirm"))
        (loop with users = (h:session-value :confirmation-data)
          for user in users do
          (a:d-remove-user *rbac* user)
          finally (h:redirect source :protocol :https))
      (h:redirect source :protocol :https))))

(h:define-easy-handler (delete-roles-handler :uri "/delete-roles"
                       :default-request-type :post)
  ((roles :parameter-type '(list string)))
  (multiple-value-bind (user allowed required-roles)
    (session-user (list *admin-role*))
    (u:log-it-pairs :debug :in "delete-roles-handler"
      :user user
      :allowed allowed
      :required-roles required-roles
      :roles-to-delete roles)
    (unless allowed
      (setf (h:return-code*) h:+http-forbidden+)
      (return-from delete-roles-handler "Forbidden"))
    (setf
      (h:session-value :confirmation-title)
      "Delete Roles"
      (h:session-value :confirmation-description)
      (s:with-html-string
        (:div :class "confirmation description"
          (:p "Please confirm that you want to delete the following roles:")
          (:ul
            (:raw (loop for role in roles collect
                    (s:with-html-string (:li role)) into html
                    finally (return (join-html html)))))))
      (h:session-value :confirmation-data) roles)
    (h:redirect (add-to-url-query "/confirm"
                  :source "/list-roles"
                  :target "/delete-roles-do")
      :protocol :https)))

(h:define-easy-handler (delete-roles-do-handler :uri "/delete-roles-do")
  (action source)
  (multiple-value-bind (user allowed required-roles)
    (session-user (list *admin-role*))
    (u:log-it-pairs :debug :in "delete-user-do-handler"
      :source source
      :user user
      :allowed allowed
      :required-roles required-roles)
    (if (and user allowed (equal action "confirm"))
      (loop with roles = (h:session-value :confirmation-data)
        for role in roles do
        (a:d-remove-role *rbac* role)
        finally (h:redirect source :protocol :https))
      (h:redirect source :protocol :https))))

(defun render-role-list (page page-size)
  (let ((headers (list "Role" "Description" "Created" "Users" "Permissions" ""))
         (rows
           (loop
             with roles = (a:list-roles-regular *rbac* page page-size)
             with admin-roles = (list *admin-role*)
             for role in roles
             for role-name = (getf role :role-name)
             for description = (getf role :role-description)
             for created = (readable-timestamp (getf role :created-at))
             for user-count = (a:list-role-users-count *rbac* role-name)
             for permissions = (a:list-role-permission-names *rbac* role-name)
             for checkbox = (input-checkbox "roles" "" :value role-name
                              :disabled (has admin-roles role-name))
             collect
             (list role-name description created user-count
               (format nil "~{~a~^, ~}" permissions) checkbox))))
    (input-form "delete-roles-form" "delete-roles-form" "/delete-roles" "post"
      (s:with-html-string
        (:raw (render-table headers rows))
        (:raw (input-submit-button "Delete Roles"))))))

(defun render-new-role-form ()
  (s:with-html-string
    (:raw (input-form "add-role" "add-role" "/add-role" "post"
            (input-text "role" "Role:" t)
            (input-text "description" "Description:" t)
            (input-checkbox-list "permissions" "Permissions:"
              (permissions-names))
            (input-submit-button "Create")))))

(defun start-web-server ()
  (setf *http-server* (make-instance 'fs-acceptor
                        :port *http-port*
                        :document-root *document-root*))
  (setf
    h:*show-lisp-errors-p* t
    (h:acceptor-persistent-connections-p *http-server*) nil)
  (u:log-it :info :in "start-web-server"
    :status "server started"
    :endpoint (format nil "http://localhost:~d" *http-port*))
  (h:start *http-server*))

(defun stop-web-server ()
  (h:stop *http-server*)
  (setf *http-server* nil))

(defun init-database ()
  (u:log-it-pairs :info :in "init-database"
    :host *db-host*
    :port *db-port*
    :db-name *db-name*
    :db-username *db-username*
    *db-host* *db-port* *db-name* *db-username*)
  ;; Create the connection
  (setf *rbac* (make-instance 'a:rbac-pg
                 :host *db-host*
                 :port *db-port*
                 :dbname *db-name*
                 :username *db-username*
                 :password *db-password*
                 :resource-regex "^/([-_.a-zA-Z0-9 ]+/)*$"))
  ;; Add admin role if it doesn't exist
  (unless (a:get-id *rbac* *roles-table* *admin-role*)
    (a:d-add-role *rbac* *admin-role*
      :description "The administrative role."))
  ;; Add admin user if they don't exist
  (unless (a:get-id *rbac* *users-table* *admin*)
    (a:d-add-user *rbac* *admin* *admin-password*
      :roles (list *admin-role*)))
  ;; Add the guest user if they don't exist
  (unless (a:get-id *rbac* *users-table* *guest*)
    (a:d-add-user *rbac* *guest* *guest-password*
      :roles (list *public-role*)))
  ;; Remove logged-in role from guest if necessary
  (when (member *logged-in-role* (db-list-roles *guest*)
          :test 'equal)
    (a:d-remove-user-role *rbac* *guest* *logged-in-role*))
  ;; Add a public root directory resource
  (unless (a:get-id *rbac* *resources-table* "/")
    (a:d-add-resource *rbac* "/" :roles (list *public-role*)))
  ;; All done. Return the connection, even though nothing is likely
  ;; to need it, for debugging and testing.
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
  (u:log-it-pairs :info :in "run" :status "Initializing database")
  ;; Initialize the database
  (let ((success (handler-case (init-database)
                   (error (condition)
                     (u:log-it :error (format nil "~a" condition))
                     nil))))
    (u:log-it-pairs :debug :in "run"
      :status "database initialized"
      :success (if success "success" "failure"))

    ;; Start the Swank server
    (unless *swank-server*
      (u:log-it-pairs :info :in "run" :status "starting swank")
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
