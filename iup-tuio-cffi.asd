(defsystem #:iup-tuio-cffi
  :pathname "tuio-cffi/"
  :components ((:file "tuio-cffi"))
  :depends-on (#:iup-cffi
	       #:cffi))
