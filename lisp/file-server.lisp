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
  (a:get-id *rbac* "resources" directory))

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

(defun menu (user subtitle)
  (let* ((roles (db-list-roles user t))
          (is-admin (member *root-role* roles :test 'equal)))
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
    (session-user '("guest" "logged-in"))
    (let* ((abs-path (u:join-paths *document-root* path))
            (path-only (clean-path path))
            (method (h:request-method*)))
      (u:log-it-pairs :debug
        :details "files-handler"
        :user user
        :allowed allowed
        :required-roles (map 'vector 'identity required-roles)
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
        (error-page "Add User" user "Failed to add user ~a. ~a" username e)))))

(h:define-easy-handler (confirm-handler :uri "/confirm")
  (source target)
  (multiple-value-bind (user allowed required-roles)
    (session-user '("logged-in"))
    (u:log-it-pairs :debug :detail "confirm-handler"
      :source source
      :target target
      :user user
      :allowed allowed
      :required-roles (map 'vector 'identity required-roles))
    (unless allowed
      (u:log-it-pairs :error :detail "confirm-handler"
        :status "Not authorized"
        :user user
        :allowed allowed
        :required-roles (map 'vector 'identity required-roles)
        :source source
        :target target)
      (setf (h:return-code*) h:+http-forbidden+)
      (return-from confirm-handler "Forbidden"))
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
    (values user allowed required-roles)))

(h:define-easy-handler (list-users-handler :uri "/list-users")
  ((page :parameter-type 'integer :init-form 1)
    (page-size :parameter-type 'integer :init-form 20))
  (multiple-value-bind (user allowed required-roles)
    (session-user '("admin"))
    (u:log-it-pairs :debug :detail "list-users-handler"
      :user user
      :allowed allowed
      :required-roles (map 'vector 'identity required-roles)
      :page page
      :page-size page-size)

    ;; Is user authorized?
    (unless allowed
      (u:log-it-pairs :info
        :details "Authorization failed"
        :user user
        :allowed allowed
        :required-roles (map 'vector 'identity required-roles)
        :error (format nil "Only ~a user is allowed" *root-username*)
        (h:redirect "/files" :protocol :https)))

    ;; Is the method GET?
    (unless (eql (h:request-method*) :get)
      (u:log-it-pairs 
        :warn
        :detail "list-users-handler"
        :status "HTTP method not supported"
        :user user
        :method (h:request-method*))
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

(h:define-easy-handler (delete-users-handler :uri "/delete-users"
                         :default-request-type :post)
  ((usernames :parameter-type '(list string)))
  (multiple-value-bind (user allowed required-roles)
    (session-user '("admin"))
    (u:log-it-pairs :debug 
      :detail "delete-users-handler"
      :user user
      :allowed allowed
      :required-roles (map 'vector 'identity required-roles)
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

(h:define-easy-handler (delete-users-do-handler :uri "/delete-users-do")
  (action source)
  (multiple-value-bind (user allowed required-roles)
    (session-user '("admin"))
    (u:log-it-pairs :debug :detail "delete-user-do-handler"
      :source source
      :user user
      :allowed allowed
      :required-roles (map 'vector 'identity required-roles))
    (if (and user allowed (equal action "confirm"))
        (loop with users = (h:session-value :confirmation-data)
          for user in users do
          (a:d-remove-user *rbac* user)
          finally (h:redirect source :protocol :https))
      (h:redirect source :protocol :https))))

(h:define-easy-handler (list-roles-handler :uri "/list-roles")
  ((page :parameter-type 'integer :init-form 1)
    (page-size :parameter-type 'integer :init-form 20))
  (multiple-value-bind (user allowed required-roles)
    (session-user '("admin"))
    (u:log-it-pairs :debug :detail "list-roles-handler"
      :user user
      :allowed allowed
      :required-roles (map 'vector 'identity required-roles)
      :page page
      :page-size page-size)

    ;; User authorized?
    (unless allowed
      (u:log-it-pairs :info
        :detail "list-roles-handler"
        :user user
        :allowed allowed
        :required-roles (map 'vector 'identity required-roles)
        :status "Authorization failed"))
    
    ;; Is the method GET?
    (unless (eql (h:request-method*) :get)
      (u:log-it-pairs 
        :warn
        :detail "list-roles-handler"
        :status "HTTP method not supported"
        :user user
        :method (h:request-method*))
      (setf (h:return-code*) h:+http-method-not-allowed+)
      (return-from list-roles-handler "Method Not Allowed"))

    (page
      (s:with-html-string
        (:div :class "role-list"
          (:raw (render-role-list page page-size))
          (:raw (render-pager "/list-roles"
                  page page-size (a:list-roles-regular-count *rbac*)))
          (:raw (render-new-role-form))))
      :user user
      :subtitle "List Roles")))

(h:define-easy-handler (add-role-handler :uri "/add-role")
  ((role)
    (description))
  (multiple-value-bind (user allowed required-roles)
    (session-user (list *root-role*))
    (U:log-it-pairs :debug :detail "add-role-handler"
      :user user
      :allowed allowed
      :required-roles required-roles
      :role role
      :description description)
    (unless allowed
      (u:log-it-pairs :error :detail "add-role-handler"
        :user user
        :allowed allowed
        :required-roles required-roles)
      (return-from add-role-handler
        (page
          (s:with-html-string
            (:p "Error: Authorization failed"))
          :subtitle "Error Adding Role"
          :user user)))
    (handler-case
      ;; Add the role
      (let ((role-id (a:d-add-role *rbac* role :description description)))
        (unless role-id
          (return-from add-role-handler
            (error-page "Add Role" user "Failed to add role '~a'" role)))
        (page
          (s:with-html-string
            (:p (format nil "Successfully added role '~a'" role)))
          :subtitle "Added Role"
          :user user))
      (error (e)
        (error-page "Add Role" user "Failed to add role '~a': ~a" role e)))))

(defun render-role-list (page page-size)
  (let ((headers (list "Role" "Description" "Created" "User Count" ""))
         (rows (loop
                 with roles = (a:list-roles-regular *rbac* page page-size)
                 for role in roles
                 for role-name = (getf role :role-name)
                 for description = (getf role :role-description)
                 for created = (readable-timestamp (getf role :created-at))
                 for user-count = (a:list-role-users-count *rbac* role-name)
                 for checkbox = (input-checkbox "roles" "" :value role-name
                                  :disabled
                                  (member role '("admin")))
                 collect 
                 (list role-name description created user-count checkbox))))
    (input-form "delete-roles-form" "delete-roles-form" "/delete-roles" "post"
      (s:with-html-string
        (:raw (render-table headers rows))
        (:raw (input-submit-button "Delete Roles"))))))

(defun render-new-role-form ()
  (s:with-html-string
    (:raw (input-form "add-role" "add-role" "/add-role" "post"
            (input-text "role" "Role:" t)
            (input-text "description" "Description:" t)
            (input-submit-button "Create")))))

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
