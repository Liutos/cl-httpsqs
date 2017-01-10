(asdf:defsystem #:cl-httpsqs
  :version "0.0.1"
  :author "Liutos <mat.liutos@gmail.com>"
  :depends-on (#:drakma)
  :components
  ((:module "src"
            :serial t
            :components ((:file "package")
                         (:file "cl-httpsqs")))))
