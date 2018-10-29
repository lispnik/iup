(defsystem #:iup-tuio
  :serial t
  :pathname "tuio/"
  :components ((:file "tuio"))
  :depends-on (#:iup-tuio-cffi
	       #:iup
	       #:cffi))
