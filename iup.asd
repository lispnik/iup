#-asdf3 (error "IUP requires ASDF 3")

(defsystem #:iup
  :description "CFFI bindings to the IUP Portable User Interface library"
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :homepage "https://github.com/lispnik/iup"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :pathname "iup/"
  :components ((:file "packages")
	       (:file "reload")
	       (:file "iup-cffi")
	       (:file "iup"))
  :depends-on (#:alexandria
	       #:cffi
	       #:iup/utils))

(defsystem #:iup/utils
  :serial t
  :pathname "utils/"
  :components ((:file "packages")
	       (:file "utils"))
  :depends-on (#:alexandria
	       #:cffi))

(defsystem #:iup
  :description "CFFI bindings to the IUP Portable User Interface library"
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :homepage "https://github.com/lispnik/iup"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :pathname "iup/"
  :components ((:file "packages")
	       (:file "reload")
	       (:file "iup-cffi")
	       (:file "iup"))
  :depends-on (#:alexandria
	       #:cffi
	       #:iup/utils))

(defsystem #:iup/scintilla
  :serial t
  :pathname "scintilla/"
  :components ((:file "packages")
	       (:file "scintilla-cffi")
	       (:file "scintilla"))
  :depends-on (#:alexandria
	       #:cffi
	       #:iup
	       #:iup/utils))

(defsystem #:iup/plot
  :serial t
  :pathname "plot/"
  :components ((:file "packages")
	       (:file "plot-cffi")
	       (:file "plot"))
  :depends-on (#:alexandria
	       #:cffi
	       #:iup
	       #:iup/utils
	       #:iup/controls
	       #:cd))

(defsystem #:iup/gl
  :serial t
  :pathname "gl/"
  :components ((:file "packages")
	       (:file "gl-cffi")
	       (:file "gl"))
  :depends-on (#:alexandria
	       #:cffi
	       #:iup
	       #:iup/utils))

(defsystem #:iup/controls
  :serial t
  :pathname "controls/"
  :components ((:file "packages")
	       (:file "controls-cffi")
	       (:file "controls"))
  :depends-on (#:alexandria
	       #:cffi
	       #:iup
	       #:iup/utils))

(defsystem #:iup/mglplot
  :serial t
  :pathname "mglplot/"
  :components ((:file "mglplot-cffi")
	       (:file "mglplot"))
  :depends-on (#:alexandria
	       #:cffi
	       #:iup
	       #:iup/utils))

(defsystem #:iup/web
  :serial t
  :pathname "web/"
  :components ((:file "packages")
	       (:file "web-cffi")
	       (:file "web"))
  :depends-on (#:alexandria
	       #:iup
	       #:iup/utils))

(defsystem #:iup/examples
  :serial t
  :pathname "examples/"
  :components ((:file "packages")
	       (:file "buttons")
	       (:file "simple-notepad")
	       (:file "web-browser")
	       (:file "sample")
	       (:file "cube")
	       (:file "teapot"))
  :depends-on (#:alexandria
	       #:iup
	       #:iup/web
	       #:iup/plot
	       #:iup/gl
	       #:iup/scintilla
	       #:cd
	       #:cl-opengl
	       #:cl-glut
	       #:cl-glu))


;;; glcontrols
