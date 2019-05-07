(defsystem #:iup-im
  :serial t
  :pathname "im"
  :components ((:file "im"))
  :depends-on (#:im
	       #:iup
	       #:iup-im-cffi
	       #:cffi))
