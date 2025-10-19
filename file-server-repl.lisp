(defparameter *db-host* (dc-eclectic:setenv "DB_HOST" "localhost"))
(defparameter *db-port* (parse-integer (dc-eclectic:setenv "DB_PORT" 30101)))
(defparameter *db-name* (dc-eclectic:setenv "DB_NAME" "fileserver"))
(defparameter *db-username* (dc-eclectic:setenv "DB_USER" "fileserver"))
(defparameter *db-password* (dc-eclectic:setenv "DB_PASSWORD" "fileserver-password"))
(defparameter *root-username* (dc-eclectic:setenv "ROOT_USER" "admin"))
(defparameter *root-password* (dc-eclectic:setenv "ROOT_PASSWORD" "admin-password-123"))
(defparameter *document-root*
  (dc-eclectic:setenv "FS_DOCUMENT_ROOT" "/home/macnod/common-lisp/file-server/files/"))
(defparameter *log-file*
  (dc-eclectic:setenv "LOG_FILE" "/home/macnod/common-lisp/file-server/logs/file-server.log"))
