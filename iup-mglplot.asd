(defsystem #:iup-mglplot
  :serial t
  :pathname "mglplot/"
  :components ((:file "mglplot"))
  :depends-on (#:iup
	       #:cffi))

