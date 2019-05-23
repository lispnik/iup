(defsystem #:iup-glcontrols-cffi
  :pathname "glcontrols"
  :components ((:file "glcontrols-cffi"))
  :depends-on (#:iup-cffi
               #:tecgraf-base
	       #:cffi))
