(require :hunchentoot)
(require :postmodern)
(require :cl-ppcre)
(require :swank)

(defpackage :file-server
  (:use :cl :hunchentoot :postmodern :cl-ppcre))
(in-package :file-server)

(require :swank)

(defparameter *server* nil)
(defparameter *logs* "/app/logs/web-server.log")
(defparameter *http-port* 8080)
(defparameter *document-root* "/app/shared-files/")
(defparameter *database-name* "file_server")
(defparameter *database-user* (sb-ext:posix-getenv "POSTGRES_USER"))
(defparameter *database-password* (sb-ext:posix-getenv "POSTGRES_PASSWORD"))
(defparameter *database-host* "localhost")
(defparameter *database-port* 5432)
(defparameter *root-user* (sb-ext:posix-getenv "ROOT_USER"))
(defparameter *root-user-password* (sb-ext:posix-getenv "ROOT_USER_PASSWORD"))
(defparameter *root-user-id* nil)
(defparameter *debug* nil)

(unless (and
          *database-user*
          *database-password*
          *root-user*
          *root-user-password*)
  (error "The following environment variables must be set: ~s"
    "DATABASE-USER, DATABASE-PASSWORD, ROOT-USER, and ROOT-PASSWORD."))

(defun dbug (message &rest params)
  (when *debug*
    (apply #'format (append `(t ,message) params))))

(defun verify-string (string regex &key ignore-case)
  "Return t if STRING matches the REGEX exactly.  Use the IGNORE-CASE
parameter if you want case-insensitve matches."
  (let ((s (format nil "~a" string)))
    (multiple-value-bind (a b)
        (scan (if ignore-case (concatenate 'string "(?is)" regex) regex) s)
      (and a b (zerop a) (= b (length s))))))

(defun join-paths (&rest path-parts)
  "Joins parameters (collected in PATH-PARTS) into a unix-like file
path, inserting slashes where necessary."
  (when (null path-parts) (return-from join-paths ""))
  (let* ((parts (loop for part in path-parts
                      for part-string = (when part (format nil "~a" part))
                      unless (or (null part-string) (zerop (length part-string)))
                        collect part-string))
         (absolute (verify-string (car parts) "^/.*$"))
         (clean-parts (remove-if
                       (lambda (p) (zerop (length p)))
                       (mapcar
                        (lambda (p) (regex-replace-all "^/|/$" p ""))
                        parts)))
         (path (format nil "~{~a~^/~}" clean-parts)))
    (format nil "~a~a" (if absolute "/" "") path)))

;; Needs tests
(defun file-exists-p (path)
  "Returns a boolean value indicating if the file specified by PATH exists."
  (let ((path (probe-file path)))
    (and path
         (not (equal (file-namestring path) "")))))

;; Needs tests
(defun directory-exists-p (path)
  "Returns a boolean value indicating if the directory specified by PATH
exists."
  (let ((path (probe-file path)))
    (and path
         (not (equal (directory-namestring path) ""))
         (equal (file-namestring path) ""))))

;; Needs tests
(defun path-type (path)
  "Returns :FILE, :DIRECTORY, or :NOT-FOUND, depending on what PATH
 points to."
  (cond ((file-exists-p path) :file)
        ((directory-exists-p path) :directory)
        (t :not-found)))

(defun path-only (filename)
  "Retrieves the path (path only, without the filename) of FILENAME."
  (declare (type (or string null) filename))
  (multiple-value-bind (match strings)
      (scan-to-strings "(.+)\/[^\/]*$" filename)
    (declare (ignore match))
    (let ((string-list (map 'list 'identity strings)))
      (if (or (null string-list)
              (null (car string-list)))
          "/"
          (elt string-list 0)))))

(defun file-name-only (filename)
  "Retrieves the filename (filename only, without the path) of FILENAME."
  (if (null filename)
      ""
      (if (stringp filename)
          (multiple-value-bind (match parts)
              (scan-to-strings "((.*)/)?([^\/]*)$" filename)
            (declare (ignore match))
            (if (not (zerop (length parts)))
                (elt parts (1- (length parts)))
                ""))
          (error "FILENAME must be a string."))))

(defun leaf-directory-only (path)
  (car (last (split "/" (string-trim "/" path)))))

(defun db-insert-root-user ()
  (let ((new-id (query "insert into users (username, password)
                          values ($1, $2)
                          on conflict (username) do nothing
                          returning id"
                  *root-user*
                  *root-user-password*
                  :single)))
    (if new-id
      new-id
      (query "select id from users
              where username = $1 and password = $2"
        *root-user*
        *root-user-password*
        :single))))

(defun db-directory-id (directory)
  "Determines if DIRECTORY exists in the database, returning the directory's ID
if it does and NIL otherwise."
  (query
    "select id
     from directories
     where directory = $1"
    directory
    :single))

(defun db-user-id (user password)
  (query
    "select id from users where username = $1 and password = $2"
    user
    password
    :single))

(defun db-directory-user-id (directory-id user-id)
  (query
    "select id from directory_users
     where directory_id = $1 and user_id = $2"
    directory-id
    user-id
    :single))

(defun db-insert-directory-user (directory-id user-id)
  (let ((dir-user-id (db-directory-user-id directory-id user-id)))
    (if dir-user-id
      dir-user-id
      (query "insert into directory_users
                (directory_id, user_id)
                values ($1, $2)
                returning id"
        directory-id user-id :single))))

(defun db-insert-directory (directory)
  (dbug "Inserting directory ~a~%" directory)
  (let* ((maybe-dir-id (db-directory-id directory))
          (dir-id (if maybe-dir-id
                    maybe-dir-id
                    (query "insert into directories (directory)
                            values ($1)
                            returning id"
                      directory
                      :single))))
    (dbug "Inserting directory->user ~a (~a) -> ~a (~a)~%"
      directory
      dir-id
      *root-user*
      *root-user-id*)
    (db-insert-directory-user dir-id *root-user-id*)
    dir-id))

(defun db-list-directories ()
  (query "select directory from directories" :column))

(defun db-delete-directory (dir)
  (query "delete from directories where directory = $1" dir))

(defun fs-list-directories ()
  (mapcar
    (lambda (dir) (subseq
                    (format nil "~a" dir)
                    (1- (length *document-root*))))
    (directory (format nil "~a**/" *document-root*))))  

(defun sync-directories ()
  (let ((fs-dirs (fs-list-directories))
         (db-dirs (db-list-directories)))
    (loop for dir in fs-dirs
      unless (db-directory-id dir) do
      (db-insert-directory dir))
    (loop for dir in db-dirs
      unless (member dir fs-dirs :test 'equal)
      do (db-delete-directory dir))))
                     
(defun clean-path (path)
  (if (equal path "/")
    path
    (let* ((path-only (path-only path))
            (clean-path (if (equal path-only "/")
                          "/"
                          (format nil "/~a/" (string-trim "/" path-only)))))
      (dbug "path-only=~a; clean-path=~a~%" path clean-path)
      clean-path)))

(defun has-access (user path)
  (if (equal path "/")
    t
    (query
      "select 1
       from directory_users du
         join directories d on d.id = du.directory_id
         join users u on u.id = du.user_id
       where
         u.username = $1
         and d.directory = $2"
      user
      (clean-path path)
      :single)))

(defun log-event (type details)
  (query
    "insert into events (event_type, event_details, created_at)
       values ($1, $2, now())"
    type
    details))

(defun list-files (abs-path)
  (let ((path (if (scan "/$" abs-path)
                abs-path
                (format nil "~a/" abs-path))))
    (mapcar 
      (lambda (p)
        (subseq (namestring p) (1- (length *document-root*))))
      (uiop:directory-files path))))

(defun list-subdirectories (abs-path)
  (let ((path (if (scan "/$" abs-path)
                abs-path
                (format nil "~a/" abs-path))))
    (mapcar
      (lambda (p)
        (subseq (namestring p) (1- (length *document-root*))))
      (uiop:subdirectories path))))

(defun list-directory (path abs-path)
  (setf (content-type*) "text/html")
  (let ((files (list-files abs-path))
         (subdirs (list-subdirectories abs-path))
         (crumbs (loop 
                   with path-parts = (cons "/"
                                       (split "/" (string-trim "/" path)))
                   with count = (length path-parts)
                   for part in path-parts
                   for index = 1 then (1+ index)
                   for last = (= index count)
                   for parent-path = part then (join-paths parent-path part)
                   for parent-name = "root" then part
                   collect (if last
                             (format nil "~a" parent-name)
                             (format nil "<a href=\"?path=~a\">~a</a>"
                               parent-path parent-name))
                   into breadcrumbs
                   finally (return (format nil "~{~a~^/~}" breadcrumbs)))))
    (dbug "List directories~%")
    (dbug "Files: ~{~a~^, ~}~%" files)
    (dbug "Subdirs: ~{~a~^, ~}~%" subdirs)
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
            (leaf-directory-only d)))
        subdirs)
      (mapcar 
        (lambda (f)
          (format nil "<li><a href=\"/files?path=~a\">~a</a></li>" 
            f 
            (file-name-only f)))
        files))))

(defun get-current-datetime ()
  (multiple-value-bind (second minute hour day month year)
      (decode-universal-time (get-universal-time))
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month day hour minute second)))

(define-easy-handler (health :uri "/health") ()
  (format nil "<html><body><h1>OK</h1>~a</body></html>~%"
    (get-current-datetime)))

(define-easy-handler (root :uri "/") ()
  (redirect "https://files.sinistercode.com/files?path=/"))

(define-easy-handler (main-handler :uri "/files") (path)
  (dbug "Processing request ~a~%" *request*)
  (dbug "Authorization header: ~a~%" (header-in "authorization" *request*))
  (multiple-value-bind (user password) (authorization)
    (unless user
      (return-from main-handler (require-authorization "File Server")))
  (let* ((abs-path (join-paths *document-root* path))
          (user-id (db-user-id user password))
          (method (request-method *request*)))
    (dbug "User=~a; UserID=~a; Password=~a; Method=~a; Path=~a; AbsPath=~a~%" 
      user user-id password method path abs-path)
    ;; Is user authorized?
    (unless user-id
      (return-from main-handler (require-authorization "File Server")))
    (dbug "User is authorized~%")
    ;; Is the method GET?
    (unless (eql method :get)
      (log-event "bad-method"
        (format nil "~a; username=~a; method=~a; path=~a;~%" 
          "Method not allowed" user method path))
        (setf (return-code *reply*) 405)
        (return-from main-handler "Method Not Allowed"))
    (dbug "Method is GET~%")
    ;; Does the file or directory exist?
    (unless (or (file-exists-p abs-path) (directory-exists-p abs-path))
      (log-event "file-not-found"
        (format nil "~a; username=~a; method=~a; path=~a;~%"
          "File not found" user method path))
      (setf (return-code *reply*) 404)
      (return-from main-handler "Not Found"))
    (dbug "File or directory exists ~a~%" abs-path)
    ;; Does the user have access to the path?
    (unless (has-access user path)
      (dbug "Access denied to path=~a for user=~a; password=~a;~%"
        path user password)
      (log-event "permission-denied"
        (dbug "~a; username=~a; method=~a; path=~a;~%"
              "User lacks access" user method path))
          (setf (return-code *reply*) 403)
      (return-from main-handler "Forbidden"))
    (dbug "User ~a has access to directory or file ~a~%"
      user path)
    ;; Access OK
    (if (eql (path-type abs-path) :directory)
      (progn
        (dbug "~a is a directory~%" path)
        (list-directory path abs-path))
      (progn
        (dbug "~a is a file~%" path)
        (handle-static-file abs-path))))))

(defun start-server ()
  (setf *server* (make-instance 'easy-acceptor
                                :port *http-port*
                                :document-root *document-root*))
  (setf
    *show-lisp-errors-p* t
    (acceptor-persistent-connections-p *server*) nil)
  (dbug "Server started on http://localhost:~a~%" *http-port*)
  (start *server*))

;; Database
(connect-toplevel
  *database-name* 
  *database-user*
  *database-password*
  *database-host*
  :port *database-port*)
(setf *root-user-id* (db-insert-root-user))

;; Swank server
(swank:create-server 
  :interface "0.0.0.0"
  :port 4005
  :style :spawn
  :dont-close t)

;; Web server
(start-server)
(loop while t do (sync-directories) (sleep 5))
