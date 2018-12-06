(defsystem #:iup-mglplot
  :serial t
  :pathname "mglplot"
  :components ((:file "mglplot"))
  :depends-on (#:iup-mglplot-cffi
	       #:iup
	       #:cffi))

