(asdf:defsystem :file-server
  :description "Simple file server with RBAC"
  :author "Donnie Cameron <macnod@gmail.com>"
  :licence "MIT License"
  :depends-on (:cl-ppcre
                :hunchentoot
                :swank
                :spinneret
                :jose
                :lass
                :postmodern
                :dc-ds
                :dc-time
                :p-log
                :rbac
                :dc-eclectic)
  :serial t
  :components ((:module "lisp"
                 :components ((:file "file-server-package")
                               (:file "utils")
                               (:file "css")
                               (:file "file-server")))))
