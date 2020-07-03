(defsystem #:iup-plot
  :serial t
  :pathname "plot"
  :components ((:file "plot"))
  :depends-on (#:iup-plot-cffi
	       #:iup
	       #:iup-controls
	       #:cd
	       #:cffi
               #:tecgraf-base))
