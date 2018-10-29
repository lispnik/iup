#-windows (error "This system can only be loaded under Windows")

(defsystem #:iup/olecontrol
  :serial t
  :pathname "olecontrol/"
  :components ((:file "olecontrol"))
  :depends-on (#:cffi
	       #:iup/olecontrol-cffi
	       #:iup
	       #:iup/utils))
