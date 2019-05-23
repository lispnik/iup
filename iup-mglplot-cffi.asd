(defsystem #:iup-mglplot-cffi
  :pathname "mglplot"
  :components ((:file "mglplot-cffi"))
  :depends-on (#:iup-cffi
               #:tecgraf-base
	       #:cffi))
