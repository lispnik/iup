(defsystem #:iup-plot-cffi
  :pathname "plot"
  :components ((:file "plot-cffi"))
  :depends-on (#:iup-cffi
	       #:cd-cffi
               #:tecgraf-base
	       #:cffi))
