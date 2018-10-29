(defsystem #:iup/olecontrol-cffi
  :pathname "olecontrol-cffi/"
  :components ((:file "olecontrol-cffi"))
  :depends-on (#:cffi
	       #:iup/iup-cffi))
