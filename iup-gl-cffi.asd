(defsystem #:iup-gl-cffi
  :pathname "gl/"
  :components ((:file "gl-cffi"))
  :depends-on (#:iup-cffi
               #:tecgraf-base
	       #:cffi))
