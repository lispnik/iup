(defsystem #:iup-plot
  :serial t
  :pathname "plot"
  :components ((:file "plot"))
  :depends-on (#:iup-plot-cffi
	       #:iup
	       #:iup-utils
	       #:iup-controls
	       #:cd
	       #:cffi))
