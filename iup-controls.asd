(defsystem #:iup-controls
  :serial t
  :pathname "controls/"
  :components ((:file "controls"))
  :depends-on (#:iup-controls-cffi
	       #:iup
	       #:iup-utils))

