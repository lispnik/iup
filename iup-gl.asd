(defsystem #:iup-gl
  :serial t
  :pathname "gl/"
  :components ((:file "gl"))
  :depends-on (#:iup-gl-cffi
	       #:iup-utils
	       #:iup
	       #:cffi))
