(defsystem #:iup-im-cffi
  :pathname "im"
  :components ((:file "im-cffi"))
  :depends-on (#:im
	       #:iup-cffi
               #:tecgraf-base
	       #:cffi))
