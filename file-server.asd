(asdf:defsystem :file-server
  :description "Simple file server with RBAC"
  :author "Donnie Cameron <macnod@gmail.com>"
  :licence "MIT License"
  :depends-on (:cl-ppcre :dc-eclectic :hunchentoot :rbac :swank)
  :serial t
  :components ((:file "file-server-package")
                (:file "file-server")))
