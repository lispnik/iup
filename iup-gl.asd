(in-package #:asdf-user)

(defsystem #:iup-gl
  :serial t
  :pathname "gl/"
  :components ((:file "gl"))
  :depends-on (#:cffi
	       #:iup
	       #:iup-gl-cffi
	       #:iup-utils))
