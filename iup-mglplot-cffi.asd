(in-package #:asdf-user)

(defsystem #:iup-mglplot-cffi
  :pathname "mglplot-cffi/"
  :components ((:file "mglplot-cffi"))
  :depends-on (#:iup-cffi
	       #:cffi))
