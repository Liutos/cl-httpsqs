(asdf:defsystem #:cl-httpsqs
  :version "0.0.1"
  :author "Liutos <mat.liutos@gmail.com>"
  :depends-on (#:drakma)
  :description "A client lib for accessing HTTPSQS written in Common Lisp"
  :licence "MIT"
  :components
  ((:module "src"
            :serial t
            :components ((:file "package")
                         (:file "cl-httpsqs")))))
