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

;; User default settings
(defparameter *default-user-settings*
  '((:setting "dark mode" :type :boolean :value nil)
     (:setting "items per page" :type :number :value 20)
     (:setting "landing page" :type :string :value "/files?path=/")))

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
(defparameter *temp-directory* (u:getenv "FS_TEMP_DIRECTORY"
                                 :default "/app/temp-files/"))
(defparameter *swank-port* (u:getenv "SWANK_PORT" :default 4005 :type :integer))

;; Logs
(defparameter *log-file* (or (u:getenv "LOG_FILE") *standard-output*))
(defparameter *log-severity-threshold*
  (intern (string-upcase (or (u:getenv "LOG_SEVERITY") "DEBUG")) :keyword))
(defparameter *log-suppress-health*
  (u:getenv "LOG_SUPPRESS_HEALTH" :type :boolean :default t))

;; Environment
(defparameter *version* (u:getenv "FILE_MANAGER_VERSION" :default "0.0"))
(defparameter *environment*
  (u:getenv "FILE_MANAGER_ENVIRONMENT" :default "unknown"))

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
        :content-type (or (h:content-type*) "unknown")
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

;; Where Hunchentoot should store temporary files during uploads
(setf h:*tmp-directory* *temp-directory*)

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

(defun ensure-immutable-user-roles (username)
  (loop with to-add = (set-difference
                        (immutable-user-roles username)
                        (user-roles username)
                        :test 'equal)
    for role in to-add
    do (a:d-add-user-role *rbac* username role)))

(defun db-add-user (username password email roles)
  (a:d-add-user *rbac* username password :roles roles :email email)
  (ensure-immutable-user-roles username)
  (create-user-settings username)
  (u:log-it-pairs :info :in "db-add-user"
    :status "added user"
    :username username
    :email email
    :roles (a:list-user-role-names *rbac* username
             :page-size *max-page-size*)))

(defun db-add-resource (resource &key roles)
  (let ((all-roles (u:distinct-values
                     (append roles (list *admin-role* *system-role*)))))
    (a:d-add-resource *rbac* resource :roles all-roles)))

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

(defun copy-file (source destination &key
                   (if-exists :supersede)
                   (buffer-size (* 64 1024)))
  "Copies SOURCE file to DESTINATION file. If DESTINATION's directories do not
exist, they are created. IF-EXISTS controls the behavior if DESTINATION already
exists, and may be :error, :new-version, :rename, :rename-and-delete,
:overwrite, :append, or :supersede. IF-EXISTS defaults to :supersede.
BUFFER-SIZE controls the size of the buffer used during the copy operation, and
defaults to 64 KB. Returns the DESTINATION path."
  (when (stringp source)
    (setf source (pathname source)))
  (when (stringp destination)
    (setf destination (pathname destination)))
  (ensure-directories-exist destination)
  (with-open-file (in source
                      :direction :input
                      :element-type '(unsigned-byte 8)
                      :if-does-not-exist :error)
    (with-open-file (out destination
                        :direction :output
                        :element-type '(unsigned-byte 8)
                        :if-exists if-exists)
      (let ((buf (make-array buffer-size :element-type '(unsigned-byte 8))))
        (loop
          (let ((pos (read-sequence buf in)))
            (when (zerop pos)
              (return))
            (write-sequence buf out :end pos))))))

  ;; Preserve modification time in a portable way (optional but nice)
  (let ((mtime (file-write-date source)))
    (when mtime
      (ignore-errors
        (sb-posix:utimes (namestring destination) mtime mtime))))
  destination)

(defun sync-directories ()
  "Ensures that directories that have been added to the file system are added to
the RBAC database, and that directories that have been removed from the file
system are removed from the RBAC database. The resources in the RBAC database
should correspond exactly to the directories in the file system."
  (u:log-it-pairs :debug :in "sync-directories")
  (let* ((fs-dirs (fs-list-directories))
          (db-dirs (resource-names))
          (added (loop
                   for dir in fs-dirs
                   unless (db-directory-id dir)
                   do (db-add-resource dir)
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
          (if (and user (not (equal user *guest*)))
            (:li (:a :href "/settings"
                   (:img :src "/image?name=user.png")
                   (:span :class "user" user)))
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
          (:link :rel "stylesheet" :href "/css?"))
        (:body
          (:div :class "main-page"
            (:raw (menu user subtitle))
            (:div :class "title" title)
            (when subtitle (:h2 subtitle))
            (:raw content)
            (:div :class "status-line"
              (:span :class "environment" "environment:" *environment*)
              (:span :class "version" "version:"*version*))))))))

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
    (if (and (not roles-show) (has raw-roles *admin-role*))
      (list *admin-role*)
      roles-show)))

(defun directory-section (user path subdirs)
  (s:with-html-string
    (:ul :class "listing"
      (loop
        with image = "/image?name=folder.png"
        and image-edit = "/image?name=edit.png"
        for dir in subdirs
        for name = (u:leaf-directory-only dir)
        for dir-roles = (format nil "~{~a~^, ~}" (directory-roles dir))
        for href = (format nil "/files?path=~a" dir)
        for edit-roles-href = (add-to-url-query "/edit-directory-roles"
                                "directory" dir
                                "parent" path)
        collect
        (:li
          (:a :href href
            (:img :src image
              :alt "Open directory" :title "Open directory"
              :width 24 :height 24)
            name)
          (:span dir-roles)
          (when (a:user-allowed *rbac* user "update" path)
            (:a :href edit-roles-href :class "edit-roles-link"
              (:img :src image-edit :alt "Edit roles" :title "Edit roles"
                :width 16 :height 16))))))))

(defun files-section (files)
  (s:with-html-string
    (:ul :class "listing"
      (mapcar
        (lambda (f)
          (:li (:a :href (format nil "/files?path=~a" f)
                 :target "_blank"
                 (:img :src "/image?name=file.png"
                   :alt "Open file" :title "Open file"
                   :width 24 :height 24)
                 (u:filename-only f))))
        files))))

(defun directory-bread-crumbs (crumbs roles)
  (s:with-html-string
    (:div :class "breadcrumb"
      (:img :src "/image?name=home.png" :width 24 :height 24)
      (:div (:raw crumbs)))
    (:div :class "access-list"
      (:img :src "/image?name=users.png" :width 16 :height 16)
      (:span (format nil "~{~a~^, ~}" roles)))))

(defun render-directory-listing (user path abs-path)
  (setf (h:content-type*) "text/html")
  (let ((files (list-files abs-path))
         (subdirs (rdl-subdirectories user abs-path))
         (crumbs (assemble-breadcrumbs path))
         (roles (directory-roles path)))
    (u:log-it-pairs :debug :in "render-directory-listing"
      :status "list directories"
      :user user
      :path path
      :abs-path abs-path
      :files files
      :subdirectories subdirs
      :access-list roles)
    (page (join-html
            (directory-bread-crumbs crumbs roles)
            (directory-section user path subdirs)
            (files-section files)
            (when (and
                    (has (user-roles user) *logged-in-role*)
                    (a:user-allowed *rbac* user "create" path))
              (list
                (render-upload-file-form path)
                (render-new-directory-form user path))))
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

(defun render-login-form (&key error-message redirect)
  (page
    (input-form "login-form" "/login-do" "post"
      (when error-message (form-text error-message :class "error-message"))
      (input-hidden "redirect" redirect)
      (input-text "Username: " :required t :autocomplete t)
      (input-text "Password: " :required t :is-password t :autocomplete t)
      (input-submit-button "Log In"))))

(h:define-easy-handler (login-handler :uri "/login")
  (error-message redirect)
  (setf (h:content-type*) "text/html")
  (u:log-it-pairs :debug :in "login-handler"
    :error error-message
    :redirect redirect)
  (when (zerop (length redirect))
    (setf redirect nil))
  (render-login-form :error-message error-message :redirect redirect))

(h:define-easy-handler (login-do-handler :uri "/login-do" :default-request-type :post)
  (username password redirect)
  (setf (h:content-type*) "text/html")
  (multiple-value-bind (user allowed required-roles)
    (session-user (list *public-role*))
    (u:log-it-pairs :debug :in "login-do-handler"
      :user user
      :allowed allowed
      :required-roles required-roles
      :username username
      :redirect redirect)
    (let (errors)
      (when (zerop (length username))
        (push "Username is required." errors))
      (when (zerop (length password))
        (push "Password is required." errors))
      (when errors
        (setf (h:return-code*) h:+http-bad-request+)
        (return-from login-do-handler
          (error-page-list :info "login-do-handler"
            (format nil "logging in '~a'" username)
            *guest*
            "There were errors with your submission"
            errors
            (list :username username :redirect redirect)))))

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
            :protocol :https))))))

(h:define-easy-handler (logout :uri "/logout") (redirect)
  (h:delete-session-value :jwt-token)
  (h:redirect (add-to-url-query "/login" "redirect" redirect) :protocol :https))

(h:define-easy-handler (js :uri "/js") ()
  (h:handle-static-file *file-server-js*))

(h:define-easy-handler (css :uri "/css") ()
  (setf (h:content-type*) "text/css")
  (multiple-value-bind (user allowed required-roles)
    (session-user (list *public-role*))
    (declare (ignore allowed required-roles))
    (generate-css user)))

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
        (u:log-it-pairs :info :in "files-handler"
          :status "authorization failed"
          :old-user user
          :new-user *guest*)
        (setf user *guest*))

      ;; Is the method GET?
      (unless (eql method :get)
        (u:log-it-pairs :warn :in "files-handler"
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
      (u:log-it-pairs :debug :in "files-handler"
        :status "file or directory exists"
        :file-or-directory abs-path)

      ;; Does the user have access to the path?
      (unless (has-read-access user path-only)
        (u:log-it-pairs :info :in "files-handler"
          :status "access denied"
          :path path :path-only path-only :user user)
        (setf (h:return-code h:*reply*) h:+http-forbidden+)
        (return-from files-handler "Forbidden"))
      (u:log-it-pairs :info :in "files-handler"
        :status "access granted" :user user :path path-only)

      ;; Access OK
      (if (eql (u:path-type abs-path) :directory)
        (progn
          (u:log-it-pairs :debug :in "files-handler"
            :status "path is a directory"
            :path path)
          (render-directory-listing user path abs-path))
        (progn
          (u:log-it-pairs :debug :in "file-handler"
            :status "path is a file"
            :path path)
          (h:handle-static-file abs-path))))))

(defun user-list-user-roles (user)
  (let ((roles (exclude-regex
                 (exclude
                   (user-roles user)
                   (list *logged-in-role*))
                 ":exclusive$"
                 nil)))
    (if (equal roles (list *public-role*))
      roles
      (exclude roles *public-role*))))

(defun render-edit-user-roles-link (user)
  (let ((path (add-to-url-query "/edit-user-roles" "user" user))
         (roles (user-list-user-roles user)))
    (s:with-html-string
      (:div :class :user-roles-cell
        (:span (format nil "~{~a~^, ~}" roles))
        (:a :href path (:img :src "/image?name=edit.png"
                         :width "16px" :height "16px"))))))

(defun render-user-list (page page-size)
  (let ((headers (list "User" "Email" "Created" "Last Login" "Roles" " "))
         (rows (loop
                 with users = (a:list-users *rbac* page page-size)
                 and excluded = (list *system*)
                 and fixed-users = (list *admin* *guest* *system*)
                 for user in users
                 for username = (getf user :username)
                 for include = (not (has excluded username))
                 for email = (getf user :email)
                 for created = (readable-timestamp (getf user :created-at))
                 for last-login = (readable-timestamp (getf user :last-login))
                 for roles = (when include
                               (render-edit-user-roles-link username))
                 for checkbox = (input-checkbox ""
                                  :name "usernames"
                                  :value username
                                  :disabled (has fixed-users username))
                 when include
                 collect (list username email created last-login roles
                           checkbox))))
    (input-form "delete-users-form" "/delete-users" "post"
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
                                     (:td (:raw (format nil "~a" value))))
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

(defun role-names-non-system (user)
  (let ((roles (exclude
                 (role-names)
                 (list
                   *logged-in-role*
                   *system-role*
                   (exclusive-role-for *system*)
                   (exclusive-role-for *guest*))))
         (user-exclusive (exclusive-role-for user)))
    (exclude-regex roles ":exclusive$" (list user-exclusive))))

(defun role-permission-names (role)
  (a:list-role-permission-names *rbac* role))

(defun regular-roles ()
  (a:list-role-names-regular *rbac* :page-size *max-page-size*))

(defun user-roles (user)
  (a:list-user-role-names *rbac* user :page-size *max-page-size*))

(defun immutable-user-roles (user)
  (list *logged-in-role* *public-role* (exclusive-role-for user)))

(defun mutable-user-roles (user)
  (exclude (user-roles user) (immutable-user-roles user)))

(defun regular-user-roles (user)
  (a:list-user-role-names-regular *rbac* user :page-size *max-page-size*))

(defun resource-names ()
  (a:list-resource-names *rbac* :page-size *max-page-size*))

(defun resource-roles (resource)
  (a:list-resource-role-names *rbac* resource :page-size *max-page-size*))

(defun regular-resource-roles (resource)
  (a:list-resource-role-names-regular *rbac* resource
    :page-size *max-page-size*))

(defun default-setting (name)
  (getf (loop for setting in *default-user-settings*
          when (equal (getf setting :setting) name)
          return setting)
    :value))

(defun user-setting (user setting)
  (let ((id (when user (a:get-id *rbac* "users" user))))
    (when (setting-exists *default-user-settings* setting)
      (if id
        (let ((serialized (a:get-value *rbac* "user_settings" "setting_value"
                            "user_id" id
                            "setting_key" setting)))
          (if serialized
            (read-from-string serialized)
            (default-setting setting)))
        (default-setting setting)))))

(defun render-new-user-form ()
  (let ((roles (regular-roles)))
    (input-form "add-user" "/add-user" "post"
      (input-text "Username:" :required t)
      (input-text "Email:" :required t)
      (input-password :required t)
      (input-checkbox-list "Roles:" roles)
      (input-submit-button "Create"))))

(defun role-options (user parent)
  (let* ((user-roles (user-roles user))
          (system-roles (list *system-role*
                          (exclusive-role-for *system*)
                          (exclusive-role-for *guest*)
                          *logged-in-role*))
          (parent-roles (if (or (equal parent "/")
                              (has (resource-roles parent) *public-role*))
                          (exclude (role-names) system-roles)
                          (resource-roles parent)))
          (exceptions (if (equal user *admin*)
                        system-roles
                        (append system-roles
                          (exclude-regex (role-names) ":exclusive$"
                            (list (exclusive-role-for user))))))
          (roles (if (has parent-roles "public")
                   (if (equal user *admin*)
                     parent-roles
                     user-roles)
                   (intersection parent-roles user-roles :test 'equal))))
    (u:log-it-pairs :debug :in "role-options"
      :user user
      :user-roles user-roles
      :parent parent
      :parent-roles parent-roles
      :roles roles)
    (if (has user-roles *admin-role*)
      roles
      (exclude-except roles ":exclusive$" exceptions))))

(defun render-new-directory-form (user parent)
  (input-form "add-directory" "/add-directory" "post"
    (form-title "Create Directory")
    (input-text "Directory Name (no slashes):" :required t :name "directory")
    (input-checkbox-list "Roles:" (role-options user parent))
    (input-hidden "parent" parent)
    (input-submit-button "Create")))

(defun render-upload-file-form (parent)
  (upload-form "upload-file" "/upload-file" "post"
    (form-title "Upload File")
    (input-hidden "parent" parent)
    (input-file "Select File:" :required t :name "file")
    (input-submit-button "Upload")))

(defun error-page (log-level in action user logging-list error-description
                    &rest params)
  (let* ((desc (apply #'format
                 (append (list nil error-description) params)))
          (message (format nil "Error while ~a: ~a" action desc))
          (body (s:with-html-string
                  (:div :class "error"
                    (:p :class "error-description" message))))
          (logs (append
                  (list log-level
                    :in in
                    :action action
                    :user user
                    :status message)
                  logging-list)))
    (apply #'u:log-it-pairs logs)
    (page body :subtitle "Error" :user (or user *guest*))))

(defun error-page-list (log-level in action user error-description
                         error-list logging-list)
  (let* ((desc (format nil "Error while ~a~a" action (if error-list ":" ".")))
          (body (s:with-html-string
                  (:div :class "error"
                    (:p :class "error-description" desc)
                    (when error-list
                      (:ul :class "error-list"
                        (loop for param in error-list
                          collect (:li :class "error-item" param)))))))
          (logs (append (list
                          log-level
                          :in in
                          :action action
                          :user user
                          :status desc
                          :error-description error-description
                          :error-list error-list)
                  logging-list)))

    (apply #'u:log-it-pairs logs)
    (page body :subtitle "Error" :user user)))


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
               (handler (format nil "~(~a~)" ',handler-name))
               (action (if (equal handler "upload-file-handler")
                         (format nil "uploading ~a '~a'"
                           ,element-name
                           (u:filename-only (second (h:parameter "file"))))
                         (format nil "adding ~a '~a'"
                           ,element-name name-param)))
               (log-pairs (append
                            (list
                              :debug
                              :in handler
                              :user user
                              :allowed allowed
                              :required-roles required-roles
                              :name-sym name-sym
                              :name-param name-param)
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
           (setf (h:return-code*) h:+http-forbidden+)
           (return-from ,handler-name
             (error-page :warn handler
               action
               user
               (list :user user :allowed allowed :required-roles required-roles)
               "Authorization failed")))

         ;; Validation
         ,@(loop for (test msg) in validation-clauses
             collect `(unless ,test
                        (return-from ,handler-name
                          (error-page :warn handler
                            action user nil ,msg))))

         ;; Add the element
         (handler-case
           (let ((id (handler-case
                       ,add-function
                       (error (e)
                         (return-from ,handler-name
                           (error-page :error handler
                             action user nil (format nil "~a" e)))))))
             (unless id
               (return-from ,handler-name
                 (error-page :error handler
                   action user nil
                   "Failed to add ~a '~a'" ,element-name name-param)))
             (success-page user "Success ~a." action))
           (error (e)
             (error-page :error handler action user nil (format nil "~a" e))))))))

(define-add-handler (add-user-handler "/add-user")
  (username email password confirm-password
    (new-roles :real-name "roles" :parameter-type '(list string)))
  (((equal password confirm-password) "Passwords don't match"))
  (db-add-user username password email new-roles)
  "user")

(define-add-handler (add-role-handler "/add-role")
  (role description (permissions :parameter-type '(list string)))
  nil
  (a:d-add-role *rbac* role :description description :permissions permissions)
  "role")

(defun absolute-directory-path (directory)
  "Converts DIRECTORY, a string representing the logical path of a directory,
which is relative to the document root and stored as a resource in the RBAC
system, into an absolute path in the file system, where files are physically
stored. This function ensures that the returned path ends with a trailing
slash."
  (let ((path (if (re:scan "/$" directory)
                directory
                (format nil "~a/" directory))))
    (concatenate 'string (u:join-paths *document-root* path) "/")))

(defun absolute-file-path (directory file)
  "Converts DIRECTORY and FILE into an absolute path in the file system,
where files are physically stored. DIRECTORY is a string representing the
logical path of a directory, relative to the document root and stored as
a resource in the RBAC system. FILE is the name of a file within that
directory."
  (u:join-paths *document-root* directory file))

(define-add-handler (add-directory-handler "/add-directory"
                      :required-roles ("logged-in"))
  (directory
    parent
    (new-roles :real-name "roles" :parameter-type '(list string)))
  (((not (re:scan "/" directory)) "Directory has slashes")
    ((or (equal parent "/") (re:scan "^/.+/$" parent))
      "Parent directory must be absolute and end in a slash")
    ((re:scan "^[-a-zA-Z0-9_]+$" directory)
      (format nil "~{~a~}"
        (list
          "Directory name '" directory "' is invalid. "
          "Use only letters, numbers, hyphens, and underscores.")))
    ((u:directory-exists-p (u:join-paths *document-root* parent))
       "Parent directory not in file system")
    ((not (has (resource-names) (concatenate 'string parent directory "/")))
      "Directory already exists")
    ((a:get-id *rbac* *users-table* user)
      (format nil "User '~a' doesn't exist" user))
    ((loop for role in new-roles always (a:get-id *rbac* *roles-table* role))
      "One or more roles doesn't exist"))
  (let* ((resource (concatenate 'string parent directory "/"))
          (absolute-path (absolute-directory-path resource))
          (all-roles (cons (format nil "~a:exclusive" user) new-roles)))
    (u:log-it-pairs :debug :in "add-directory-handler"
      :user user
      :resource resource
      :absolute-path absolute-path
      :all-roles all-roles)
    (ensure-directories-exist absolute-path)
    (db-add-resource resource :roles all-roles))
  "directory")

(define-add-handler (upload-file-handler "/upload-file"
                      :required-roles ("logged-in"))
  (parent)
  (((or
      (search "multipart/form-data" (h:header-in* :content-type))
      (search "application/x-www-form-urlencoded" (h:header-in* :content-type)))
     "Content-Type must be multipart/form-data"))
  (let* ((file (h:post-parameter "file" h:*request*))
          (parent (h:post-parameter "parent" h:*request*))
          (temp-path (first file)) ;; Type PATHNAME
          (original-filename (second file))
          (content-type (third file))
          (new-path (absolute-file-path parent original-filename)))
    ;; ((a:get-id *rbac* "resources" (concatenate 'string parent "/"))
    ;;   (format nil "Parent directory '~a' doesn't exist"
    ;;     (concatenate 'string parent "/")))
    ;; ((a:user-allowed *rbac* user "create" (concatenate 'string parent "/"))
    ;;   (format nil "User '~a' not allowed to upload to '~a'" user parent))
    ;; ((and file (listp file) (= (length file) 3))
    ;;   "File upload is invalid"))
    (u:log-it-pairs :debug :in "upload-file"
      :user user
      :temp-path (file-namestring temp-path)
      :original-filename (file-namestring original-filename)
      :content-type content-type
      :new-path new-path)
    (copy-file temp-path (pathname new-path))
    (u:log-it-pairs :info :in "upload-file-handler"
      :status "file uploaded"
      :new-path new-path
      :content-type content-type))
  "file")

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
                    description
                    (input-form
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
                        :value "confirm"))))))
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

(defun login (username password)
  (let ((user-id (db-user-id username password)))
    (if user-id
      (let ((token (issue-jwt user-id)))
        (u:log-it-pairs :info :in "login"
          :status "login successful" :user username)
        (h:start-session)
        (setf (h:session-value :jwt-token) token))
      (progn
        (h:delete-session-value :jwt-token)
        (u:log-it-pairs :warn :in "login"
          :status "login failed" :user username)
        nil))))

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
           (return-from ,handler-name
             (error-page :warn handler action user
               (list :allowed allowed :required-roles required-roles)
               "Authorization failed")))

         ;; Is the method GET?
         (unless (eql (h:request-method*) :get)
           (return-from ,handler-name
             (error-page :warn handler
               action user
               (list :method (h:request-method*)
                 :allowed allowed
                 :reason "HTTP method not supported"
                 :required-roles required-roles)
               "Method not allowed")))

         ;; Assemble page
         (page
           (s:with-html-string
             (:div :class (format nil "~a-list" (label-to-name ,list-name))
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
  (render-role-list-count)
  (render-new-role-form)
  "roles")

(define-list-handler (list-users-handler "/list-users")
  ((page :parameter-type 'integer :init-form 1)
    (page-size :parameter-type 'integer :init-form 20))
  (render-user-list page page-size)
  (a:list-users-count *rbac*)
  (render-new-user-form)
  "users")

(define-list-handler (list-role-users-handler "/list-role-users")
  (role
    (page :parameter-type 'integer :init-form 1)
    (page-size :parameter-type 'integer :init-form 20))
  (render-role-user-list role page page-size)
  (a:list-role-users-count *rbac* role)
  (render-new-role-user-form role)
  "role users")

(defun render-role-user-list (role page page-size)
  (let ((headers (list "User" "Email" "Created" "Last Login" "Other Roles" " "))
         (rows (loop with users = (a:list-role-users *rbac* role page page-size)
                 for user in users
                 for username = (getf user :username)
                 for email = (getf user :email)
                 for created = (readable-timestamp (getf user :created-at))
                 for last-login = (readable-timestamp (getf user :last-login))
                 for other-roles = (exclude
                                     (user-list-user-roles username)
                                     role)
                 for checkbox = (input-checkbox ""
                                  :name "usernames"
                                  :value username
                                  :disabled (equal *admin* username))
                 collect (list username email created last-login other-roles
                           checkbox))))
    (input-form "delete-role-users-form" "/delete-role-users" "post"
      (s:with-html-string
        (:raw (render-table headers rows))
        (:raw (input-submit-button "Delete Users from Role"))))))

(defun render-new-role-user-form (role)
  (input-form "add-role-user-form" "/add-role-user" "post"
    (input-hidden "role" role)
    (input-text "Username:" :required t)
    (input-submit-button "Add to Role")))

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

(h:define-easy-handler (edit-directory-roles-handler :uri "/edit-directory-roles"
                          :default-request-type :get)
    (directory parent)
    (multiple-value-bind (user allowed required-roles)
      (session-user (list *logged-in-role*))
      (u:log-it-pairs :debug :in "edit-directory-roles-handler"
        :user user
        :allowed allowed
        :required-roles required-roles
        :directory directory
        :parent parent)
      (unless allowed
        (setf (h:return-code*) h:+http-forbidden+))
      (page
        (render-edit-directory-roles-form parent directory user)
        :user user
        :subtitle (format nil "Edit Roles for Directory ~a" directory))))

(h:define-easy-handler (edit-directory-roles-do-handler :uri "/edit-directory-roles-do"
                             :default-request-type :post)
  (directory parent (roles :parameter-type '(list string)))
  (multiple-value-bind (user allowed required-roles)
    (session-user (list *logged-in-role*))
    (edit-directory-roles-do-helper
      directory parent roles user allowed required-roles)))

(defun edit-directory-roles-do-helper
  (directory parent roles user allowed required-roles)
    (u:log-it-pairs :debug :in "edit-directory-roles-do-helper"
      :user user
      :allowed allowed
      :required-roles required-roles
      :directory directory
      :parent parent
      :roles roles)
    (unless allowed
      (setf (h:return-code*) h:+http-forbidden+)
      (return-from edit-directory-roles-do-helper "Forbidden"))
    (let* ((parent-roles (resource-roles parent))
            (user-roles (user-roles user))
            (allowed-roles (role-options user parent))
            (unknown-roles (exclude roles allowed-roles)))
      (u:log-it-pairs :debug :in "edit-directory-roles-do-helper"
        :parent-roles parent-roles
        :user-roles user-roles
        :allowed-roles allowed-roles
        :unknown-roles unknown-roles)
      ;; Are all the specified roles accessible to the user?
      (when unknown-roles
        (return-from edit-directory-roles-do-helper
          (error-page :error "edit-directory-roles-do-helper"
            "editing directory roles" user
            (list
              :parent-roles parent-roles
              :user-roles user-roles
              :allowed-roles allowed-roles
              :unknown-roles unknown-roles)
            "One or more roles are inaccessible or don't exist: ~{~a~^, ~}"
            unknown-roles)))

      ;; Add and remove roles as needed
      (let* ((existing-roles (resource-roles directory))
              (to-add (set-difference roles existing-roles :test 'equal))
              (to-remove (set-difference existing-roles roles :test 'equal)))
        (u:log-it-pairs :debug :in "edit-directory-roles-do-helper"
          :existing-roles existing-roles
          :to-add to-add
          :to-remove to-remove)
        ;; Add roles to the directory
        (loop for role in to-add do
          (a:d-add-resource-role *rbac* directory role))
        ;; Remove roles from the directory
        (loop for role in to-remove do
          (a:d-remove-resource-role *rbac* directory role))
        (success-page user "Updated roles for directory ~a." directory))))

(defun undeletable-roles ()
  (append
    (list
      *admin-role*
      *public-role*
      *system-role*)
    (remove-if-not (lambda (r) (re:scan ":exclusive$" r)) (role-names))))

(defun excluded-from-role-list ()
  (list
    (exclusive-role-for *public-role*)
    *logged-in-role*
    (exclusive-role-for *logged-in-role*)
    *system-role*
    (exclusive-role-for *system-role*)))

(defun render-role-list-count ()
  (length
    (exclude
      (role-names)
      (excluded-from-role-list))))

(h:define-easy-handler (edit-user-roles-handler :uri "/edit-user-roles"
                       :default-request-type :get)
  ((roles-user :real-name "user"))
  (multiple-value-bind (user allowed required-roles)
    (session-user (list *admin*))
    (u:log-it-pairs :debug :in "edit-user-roles-handler"
      :user user
      :allowed allowed
      :required-roles required-roles
      :roles-user roles-user)
    (unless allowed
      (setf (h:return-code*) h:+http-forbidden+)
      (return-from edit-user-roles-handler "Fail"))
    (page
      (render-edit-user-roles-form roles-user)
      :user user
      :subtitle (format nil "Edit Roles for User ~a" roles-user))))

(h:define-easy-handler (edit-user-roles-do-handler :uri "/edit-user-roles-do"
                         :default-request-type :post)
  ((target-user :real-name "user")
    (target-roles :real-name "roles" :parameter-type '(list string)))
  (multiple-value-bind (user allowed required-roles)
    (session-user (list *admin-role*))
    (u:log-it-pairs :debug :in "edit-user-roles-do-handler"
      :user user
      :required-roles required-roles
      :allowed allowed
      :target-user target-user
      :target-roles target-roles)
    (unless allowed
      (setf (h:return-code*) h:+http-forbidden+)
      (return-from edit-user-roles-do-handler "Forbidden"))
    (let* ((existing-roles (mutable-user-roles target-user))
            (to-add (set-difference target-roles existing-roles
                      :test 'equal))
            (to-remove (set-difference existing-roles target-roles
                         :test 'equal)))
      (u:log-it-pairs :debug :in "edit-user-roles-do-handler"
        :existing-roles existing-roles
        :to-add to-add
        :to-remove to-remove)
      ;; Add roles to the user
      (loop for role in to-add do
        (a:d-add-user-role *rbac* target-user role))
      ;; Remove roles from user
      (loop for role in to-remove do
        (a:d-remove-user-role *rbac* target-user role))
      (success-page user "Updated roles for user ~a." target-user))))

(defun render-role-list (page page-size)
  (let ((headers (list "Role" "Description" "Created" "Users" "Permissions" ""))
         (rows
           (loop
             with roles = (a:list-roles *rbac* page page-size)
             for role in roles
             for role-name = (getf role :role-name)
             for include = (not (has (excluded-from-role-list) role-name))
             for description = (getf role :role-description)
             for created = (readable-timestamp (getf role :created-at))
             for user-count = (when include
                                (a:list-role-users-count *rbac* role-name))
             for permissions = (when include
                                 (a:list-role-permission-names *rbac* role-name))
             for checkbox = (when include
                              (input-checkbox "" :name "roles" :value role-name
                              :disabled (has (undeletable-roles) role-name)))
             when include collect
             (list role-name description created user-count
               (format nil "~{~a~^, ~}" permissions) checkbox))))
    (input-form "delete-roles-form" "/delete-roles" "post"
      (s:with-html-string
        (:raw (render-table headers rows))
        (:raw (input-submit-button "Delete Roles"))))))

(defun render-new-role-form ()
  (s:with-html-string
    (:raw (input-form "add-role" "/add-role" "post"
            (input-text "Role:" :required t)
            (input-text "Description:" :required t)
            (input-checkbox-list "Permissions:" (permission-names))
            (input-submit-button "Create")))))

(defun render-edit-directory-roles-form (parent directory user)
  "Renders a form to edit the roles for DIRECTORY as USER. The form lists all the roles
that USER can assign to FILE, which depend on USER and PARENT. A user can only assign
roles to a directory that they themselves have and that are assigned to the parent
directory. The form displays the roles with with checkboxes for each role. These
checkboxes are checked if the role is currently assigned to DIRECTORY."
  (let* ((roles (role-options user parent))
          (checked (loop for role in roles
                     for checked = (has (resource-roles directory) role)
                     collect checked into checked-states
                     finally (return checked-states))))
    (u:log-it-pairs :debug :in "render-edit-directory-roles-form"
      :parent parent :directory directory :user user :roles roles)
    (input-form "edit-directory-roles" "/edit-directory-roles-do" "post"
      (input-hidden "parent" parent)
      (input-hidden "directory" directory)
      (input-checkbox-list "Roles:" roles :checked checked)
      (input-submit-button "Update"))))

(defun user-role-edit-disabled (user role)
  (cond
    ((equal (exclusive-role-for user) role) t)
    ((equal role *public-role*) t)
    ((and (equal role *admin-role*) (equal user *admin*)) t)
    (t nil)))

(defun render-edit-user-roles-form (user)
  (let* ((roles (role-names-non-system user))
          (user-roles (user-roles user))
          (checked (mapcar (lambda (r) (has user-roles r)) roles))
          (disabled (mapcar
                      (lambda (r) (user-role-edit-disabled user r))
                      roles))
          (role-states (loop for role in roles
                         for check in checked
                         for disable in disabled
                         collect
                         (list :role role :checked check :disabled disable)))
          (sorted (sort role-states
                    (lambda (a b)
                      (string<
                        (format nil "~a-~a-~a"
                          (if (getf a :disabled) "a" "b")
                          (if (getf a :checked) "a" "b")
                          (getf a :role))
                        (format nil "~a-~a-~a"
                          (if (getf b :disabled) "a" "b")
                          (if (getf b :checked) "a" "b")
                          (getf b :role)))))))
    (input-form "edit-user-roles" "/edit-user-roles-do" "post"
      (input-hidden "user" user)
      (input-checkbox-list "Roles:"
        (mapcar (lambda (r) (getf r :role)) sorted)
        :checked (mapcar (lambda (r) (getf r :checked)) sorted)
        :disabled (mapcar (lambda (r) (getf r :disabled)) sorted))
      (input-submit-button "Update"))))

(h:define-easy-handler (settings-handler :uri "/settings"
                         :default-request-type :get)
  (message)
  (multiple-value-bind (user allowed required-roles)
    (session-user (list *logged-in-role*))
    (u:log-it-pairs :debug :in "settings-handler"
      :user user
      :allowed allowed
      :required-roles required-roles)
    (unless allowed
      (h:redirect "/login" :protocol :https)
      (return-from settings-handler "Forbidden"))
    (let ((settings (a:with-rbac (*rbac*)
                      (db:query "select
                                   us.setting_key,
                                   us.setting_value
                                 from user_settings us
                                 join users u on us.user_id = u.id
                                 where u.username = $1
                                 order by us.setting_key"
                        user))))
      (page
        (input-form "settings-form" "/settings-do" "post"
          (when message (form-text message :class "settings-message"))
          (loop for (key serialized-value) in settings
            for value = (read-from-string serialized-value)
            for display-key = (format nil "~a:" key)
            for field = (if (member value '(t nil))
                          (input-checkbox-pre display-key :checked value)
                          (input-text display-key :value value))
            collect field into fields
            finally (return (join-html fields)))
          (input-password)
          (input-text "email")
          (input-submit-button "Apply Changes")
          (s:with-html-string
            (:div :class "logout-link"
              (:a :href "/logout" "Log Out"))))
        :user user
        :subtitle "Settings"))))

(defun log-pairs-from-post-parameters (starting-list)
  "Creates a plist from Hunchentoot's current POST parameters, appends
STARTING-LIST to that, and returns the result. This is convenient when
calling U:LOGIT-PAIRS from an HTTP request handler."
  (loop for (key . value) in (h:post-parameters*)
    for keyword = (intern (string-upcase (label-to-name key)) "KEYWORD")
    append (list keyword value) into pairs
    finally (return (append starting-list pairs))))

(defun remove-null-plist-values (plist)
  "Removes key/value pairs from PLIST when the value is NIL."
  (loop
    for key in plist by #'cddr
    for value in (cdr plist) by #'cddr
    when value
    append (list key value)))

(defun log-pairs-from-list (starting-list list)
  (loop
    for key in list by #'cddr
    for value in (cdr list) by #'cddr
    for keyword = (intern (string-upcase (label-to-name key)) "KEYWORD")
    append (list keyword value) into pairs
    finally (return (append (remove-null-plist-values starting-list) pairs))))

(defparameter *last-post-parameters* nil)

(defun compute-field-submission (name raw-value type default)
  (let ((zero-length (zerop (length raw-value))))
    (cond
      ((eql type :boolean)
        (if raw-value t nil))
      ((and (eql type :number) (not zero-length))
        (if (re:scan "^[0-9]+$" raw-value)
          (read-from-string raw-value)
          (progn
            (u:log-it-pairs :error
              :in "settings-do-handler"
              :status "Invalid number submission"
              :field name
              :expected-type type
              :value raw-value)
            nil)))
      ((and (eql type :string) (not zero-length))
        raw-value)
      (t default))))

(defun include-user-table-settings ()
  (append
    *default-user-settings*
    `((:setting "password" :type :string :value nil)
       (:setting "confirm-password" :type :string :value nil)
       (:setting "email" :type :string :value nil))))

(h:define-easy-handler (settings-do-handler :uri "/settings-do"
                            :default-request-type :post)
  ()
  (setf *last-post-parameters* (h:post-parameters*))
  (multiple-value-bind (user allowed required-roles)
    (session-user (list *logged-in-role*))
    (apply #'u:log-it-pairs (log-pairs-from-post-parameters
                              (list :debug :in "settings-do-handler"
                                :user user
                                :allowed allowed
                                :required-roles required-roles)))
    (unless allowed
      (setf (h:return-code*) h:+http-forbidden+)
      (return-from settings-do-handler
        (error-page :warn "settings-do-handler" "updating settings" user
          (list :user user :allowed allowed :required-roles required-roles)
          "Authorization failed")))
    (loop
      with params = (h:post-parameters*)
      with settings = (include-user-table-settings)
      with known-keys = (mapcar
                          (lambda (x) (label-to-name (getf x :setting)))
                          settings)
      for setting in settings
      for key = (getf setting :setting)
      for type = (getf setting :type)
      for default = (getf setting :value)
      for field-name = (label-to-name key)
      for raw-value = (u:trim (cdr (assoc field-name params :test 'equal)))
      for value = (compute-field-submission key raw-value type default)
      for final-value = (or value default)
      do (u:log-it-pairs :debug :in "settings-do-handler"
           :key key
           :type type
           :value default
           :raw-submitted-value raw-value
           :submitted-value value
           :final-value final-value)
      collect (list :setting key :type type :value final-value)
      into settings-to-update
      finally
      (multiple-value-bind (errors new-settings)
        (process-settings settings-to-update)
      (loop
        for new-setting in new-settings
        for k = (getf new-setting :setting)
        for v = (getf new-setting :value)
        do (update-user-setting user k v user)
        appending (list k v) into updated-settings
        finally
        (apply #'u:log-it-pairs
          (log-pairs-from-list
            (list :info :in "settings-do-handler"
              :status (if errors "some settings updated" "settings updated")
              :errors errors
              :user user)
            updated-settings))
        (h:redirect
          (add-to-url-query "/settings" "message" "Settings updated")
          :protocol :https))))))

(defun start-web-server ()
  (setf *http-server* (make-instance 'fs-acceptor
                        :port *http-port*
                        :document-root *document-root*))
  (setf
    h:*show-lisp-errors-p* t
    (h:acceptor-persistent-connections-p *http-server*) nil)
  (u:log-it-pairs :info :in "start-web-server"
    :status "server started"
    :endpoint (format nil "http://localhost:~d" *http-port*))
  (h:start *http-server*))

(defun stop-web-server ()
  (h:stop *http-server*)
  (setf *http-server* nil))

(defun setting-keys (settings)
  (mapcar (lambda (s) (getf s :setting)) settings))

(defun setting-exists (settings key)
  (when (member key (setting-keys settings) :test 'equal)) t)

(defun setting-value (settings key)
  (getf (car (remove-if-not
               (lambda (s) (equal key (getf s :setting)))
               settings))
    :value))

(defun put-setting (settings key value)
  (if (setting-exists settings key)
    (loop for setting in settings
      for setting-key = (getf setting :setting)
      when (equal setting-key key)
      do (setf (getf setting :value) value)
      finally (return settings))))

(defun remove-setting (settings &rest keys)
  (loop for setting in settings
    unless (member (getf setting :setting) keys :test 'equal)
    collect setting))

(defun process-settings-password (settings)
  (let* (errors
          (processed
             (cond
               ((and (setting-exists settings "password")
                  (setting-exists settings "confirm-password"))
                 (if (equal (setting-value settings "password")
                       (setting-value settings "confirm-password"))
                   (remove-setting settings "confirm-password")
                   (progn
                     (push "Passwords don't match. Password not changed." errors)
                     (remove-setting settings "password" "confirm-password"))))
               ((setting-exists settings "password")
                 (push "No confirm-password field. Password not changed." errors)
                 (remove-setting settings "password"))
               ((setting-exists settings "confirm-password")
                 (push (format nil "~a ~a"
                         "confirm-password field with no password field."
                         "Password not changed.")
                   errors)
                 (remove-setting settings "confirm-password"))
               (t settings))))
    (values errors processed)))

(defun process-settings-email (settings)
  (let* (errors
          (processed
            (if (setting-exists settings "email")
              (if (a:valid-email-p *rbac* (setting-value settings "email"))
                settings
                (progn
                  (push "Invalid email. Email not changed." errors)
                  (remove-setting settings "email")))
              settings)))
    (values errors processed)))

(defun process-settings (settings)
  (loop
    with settings-processors = (list
                                 #'process-settings-password
                                 #'process-settings-email)
    for processor in settings-processors
    for current-settings = settings then new-settings
    for (errors new-settings) = (multiple-value-bind (errors psettings)
                                  (funcall processor current-settings)
                                  (list errors psettings))
    when errors collect errors into all-errors
    finally (return (values all-errors new-settings))))

(defun serialize (value)
  (format nil "~s" value))

(defun update-settings-sql (key value user actor)
  (let ((user-id (a:get-id *rbac* "users" user))
         (actor-id (a:get-id *rbac* "users" actor)))
    (when (and user-id actor-id)
      (cond
        ((equal key "password")
          (list
            "update users set password_hash = $1, updated_by = $2 where id = $3"
            (a:password-hash user value) actor-id user-id))
        ((equal key "email")
          (list
            "update users set email = $1, updated_by = $2 where id = $3"
            value actor-id user-id))
        ((setting-exists *default-user-settings* key)
          (list
            (a:usql
              "insert into user_settings
              (user_id, setting_key, setting_value, updated_by)
            values ($1, $2, $3, $4)
            on conflict (user_id, setting_key)
            do update set
              setting_value = excluded.setting_value,
              updated_by = excluded.updated_by,
              updated_at = now()")
            user-id key (serialize value) actor-id))
        (t
          (u:log-it-pairs :error :in "settings-sql"
            :status "unknown setting" :setting key)
          nil)))))

(defun update-user-setting (user key value actor)
  (let ((query (update-settings-sql key value user actor)))
    (if query
      (handler-case
        (progn
          (a:with-rbac (*rbac*) (a:rbac-query query))
          (u:log-it-pairs :debug :in "update-user-setting" :status "success"
            :key key :value value :user user :actor actor)
          t)
        (error (e)
          (u:log-it-pairs :error :in "update-user-setting"
            :status "fail" :error (format nil "~a" e)
            :key key :value value :user user :actor actor)
          nil))
      (progn
        (u:log-it-pairs :error :in "update-user-setting"
          :status "fail" :error "unknown user, actor, or setting"
          :key key :value value :user user :actor actor)
        nil))))

(defun create-user-settings (user)
  (loop with user-id = (or (a:get-id *rbac* "users" user)
                         (progn
                           (u:log-it-pairs :error :in "create-user-settings"
                             :status "unknown user" :user user)
                           (return-from create-user-settings nil)))
    for setting in *default-user-settings*
    for exists = (a:get-value *rbac* "user_settings" "id"
                   "user_id" user-id
                   "setting_key" (getf setting :setting))
    unless exists
    do (update-user-setting user (car setting) (cadr setting) *admin*)
    and collect (car setting)))

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
    (db-add-user *admin* *admin-password* "no-email"
      (list *admin-role*)))
  ;; Add the guest user if they don't exist
  (unless (a:get-id *rbac* *users-table* *guest*)
    (db-add-user *guest* *guest-password* "no-email"
      (list *public-role*)))
  ;; Remove logged-in role from guest if necessary
  (when (member *logged-in-role* (user-roles *guest*) :test 'equal)
    (a:d-remove-user-role *rbac* *guest* *logged-in-role*))
  ;; Fix permissions for guest exclusive role (we want read only)
  (loop with exclusive-role = (exclusive-role-for *guest*)
    for permission in (exclude (permission-names) "read")
    when (has (role-permission-names exclusive-role) permission)
    do (a:d-remove-role-permission *rbac* exclusive-role permission))
  ;; Add a public root directory resource
  (unless (a:get-id *rbac* *resources-table* "/")
    (db-add-resource "/" :roles (list *public-role* *logged-in-role*)))
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
