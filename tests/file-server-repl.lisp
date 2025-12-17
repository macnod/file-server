(require :asdf)
(require :cl-ppcre)
(require :spinneret)
(require :babel)
(require :lass)
(require :postmodern)
(require :uiop)
(require :rbac)
(require :dc-ds)
(require :dc-time)
(require :p-log)
(require :dc-eclectic)

(dc-eclectic:setenv "DB_HOST" "127.0.0.1")
(dc-eclectic:setenv "DB_PORT" "5435")
(dc-eclectic:setenv "DB_NAME" "fileserver")
(dc-eclectic:setenv "DB_USER" "fileserver")
(dc-eclectic:setenv "DB_PASSWORD" "fileserver-password")
(dc-eclectic:setenv "ADMIN_PASSWORD" "admin-password-1234")
(dc-eclectic:setenv "JWT_SECRET" "yhQ5PFnwwVGTEOQ6pujBVwq9EPZ0wlWU")
(dc-eclectic:setenv "WEB_DIRECTORY" "$(pwd)/tests/web")
(dc-eclectic:setenv "FS_DOCUMENT_ROOT" "$(pwd)/tests/files")
(dc-eclectic:setenv "FS_TEMP_DIRECTORY" "$(pwd)/tests/temp-files")
(dc-eclectic:setenv "LOG_FILE" "$(pwd)/tests/tests.log")
(dc-eclectic:setenv "HTTP_PORT" "8090")

(push (uiop:getcwd) asdf:*central-registry*)
(ql:register-local-projects)
(asdf:load-system :file-server)

(in-package :file-server)

(setf *swank-server*
  (swank:create-server
    :interface "0.0.0.0"
    :port *swank-port*
    :style :spawn
    :dont-close t))
