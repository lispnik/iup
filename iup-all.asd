(defsystem #:iup-all
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :homepage "https://github.com/lispnik/iup"
  :licence "MIT"
  :depends-on (#:iup
	       #:iup-cd
	       #:iup-controls
	       #:iup-classesdb
	       #:iup-gl
	       #:iup-glcontrols
	       #:iup-im
	       #:iup-imglib
	       #:iup-mglplot
	       #:iup-plot
	       #+windows #:iup-olecontrol
	       #:iup-scintilla
	       #:iup-tuio
	       #:iup-web
	       #:iup-threads))
