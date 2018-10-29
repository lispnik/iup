(in-package #:asdf-user)

(defsystem #:iup-classesdb-cffi
  :pathname "classesdb-cffi/"
  :components ((:file "classesdb-cffi"))
  :depends-on (#:cffi))
