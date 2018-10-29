(in-package #:asdf-user)

(defsystem #:iup-gl-cffi
  :pathname "gl-cffi/"
  :components ((:file "gl-cffi"))
  :depends-on (#:cffi
	       #:iup-cffi))
