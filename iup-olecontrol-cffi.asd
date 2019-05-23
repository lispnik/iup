(defsystem #:iup-olecontrol-cffi
  :pathname "olecontrol"
  :components ((:file "olecontrol-cffi"))
  :depends-on (#:iup-cffi
               #:tecgraf-base
	       #:cffi))
