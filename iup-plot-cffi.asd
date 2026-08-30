(defsystem #:iup-plot-cffi
  :pathname "plot"
  :components ((:file "plot-cffi"))
  :depends-on (#:iup-cffi
	       #:cd
               #:tecgraf-base
	       #:cffi))
