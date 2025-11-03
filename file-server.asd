(asdf:defsystem :file-server
  :description "Simple file server with RBAC"
  :author "Donnie Cameron <macnod@gmail.com>"
  :licence "MIT License"
  :depends-on (:cl-ppcre
                :dc-eclectic
                :hunchentoot
                :rbac
                :swank
                :spinneret
                :jose
                :lass
                :dc-ds)
  :serial t
  :components ((:module "lisp"
                 :components ((:file "file-server-package")
                               (:file "utils")
                               (:file "css")
                               (:file "file-server")))))
