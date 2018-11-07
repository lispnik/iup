(defsystem #:iup-controls-cffi
  :pathname "controls/"
  :components ((:file "controls-cffi"))
  :depends-on (#:iup-cffi
	       #:cffi))
