(defsystem #:iup-plot-cffi
  :pathname "plot-cffi/"
  :components ((:file "plot-cffi"))
  :depends-on (#:iup-cffi
	       #:cd-cffi
	       #:cffi))
