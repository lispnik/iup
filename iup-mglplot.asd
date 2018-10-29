(in-package #:asdf-user)

(defsystem #:iup-mglplot
  :serial t
  :pathname "mglplot/"
  :components ((:file "mglplot"))
  :depends-on (#:cffi
	       #:iup))

