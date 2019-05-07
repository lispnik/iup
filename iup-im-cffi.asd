(defsystem #:iup-im-cffi
  :pathname "im"
  :components ((:file "im-cffi"))
  :depends-on (#:im-cffi
	       #:iup-cffi
	       #:cffi))
