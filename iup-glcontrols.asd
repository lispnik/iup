(in-package #:asdf-user)

(defsystem #:iup-glcontrols
  :serial t
  :pathname "glcontrols/"
  :components ((:file "glcontrols"))
  :depends-on (#:alexandria
	       #:cffi
	       #:iup-glcontrols-cffi
	       #:iup-utils))
