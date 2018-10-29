(in-package #:asdf-user)

(defsystem #:iup-cffi
  :pathname "iup-cffi/"
  :components ((:file "iup-cffi"))
  :depends-on (#:cffi))
