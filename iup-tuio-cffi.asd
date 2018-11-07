(defsystem #:iup-tuio-cffi
  :pathname "tuio/"
  :components ((:file "tuio-cffi"))
  :depends-on (#:iup-cffi
	       #:cffi))
