(defsystem #:iup-glcontrols
  :serial t
  :pathname "glcontrols/"
  :components ((:file "glcontrols"))
  :depends-on (#:iup-glcontrols-cffi
	       #:iup-utils
               #:iup
	       #:alexandria
	       #:cffi))
