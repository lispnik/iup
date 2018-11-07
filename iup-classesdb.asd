(defsystem #:iup-classesdb
  :serial t
  :pathname "classesdb/"
  :components ((:file "classesdb"))
  :depends-on (#:iup-classesdb-cffi
	       #:iup-cffi
	       #:iup-controls-cffi
	       #:iup-plot-cffi
	       #:iup-mglplot-cffi
	       #:iup-gl-cffi
	       #:iup-glcontrols-cffi
	       #+windows #:iup-olecontrol-cffi
	       #:iup-scintilla-cffi
	       #:iup-web-cffi
	       #:iup-tuio-cffi
	       #:trivial-features
	       #:local-time))
