(in-package #:asdf-user)

(defsystem #:iup-glcontrols-cffi
  :pathname "glcontrols-cffi/"
  :components ((:file "glcontrols-cffi"))
  :depends-on (#:cffi
	       #:iup-cffi))
