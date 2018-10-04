#-asdf3 (error "IUP requires ASDF 3")

(defsystem #:iup
  :description "CFFI bindings to the IUP Portable User Interface library"
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :pathname "iup/"
  :components ((:file "packages")
	       (:file "iup-cffi")
	       (:file "classes")
	       (:file "iup"))
  :depends-on (#:alexandria
	       #:cffi))

(defsystem #:iup/web
  :serial t
  :pathname "web/"
  :components ((:file "packages")
	       (:file "web-cffi")
	       (:file "web"))
  :depends-on (#:alexandria
	       #:cffi
	       #:iup))

(defsystem #:iup/scintilla
  :serial t
  :pathname "scintilla/"
  :components ((:file "packages")
	       (:file "scintilla-cffi")
	       (:file "scintilla"))
  :depends-on (#:alexandria
	       #:cffi
	       #:iup))

(defsystem #:iup/plot
  :serial t
  :pathname "plot/"
  :components ((:file "plot-cffi")
	       (:file "plot"))
  :depends-on (#:alexandria
	       #:cffi
	       #:iup))

(defsystem #:iup/mglplot
  :serial t
  :pathname "mglplot/"
  :components ((:file "mglplot-cffi")
	       (:file "mglplot"))
  :depends-on (#:alexandria
	       #:cffi
	       #:iup))


;;; gl
;;; glcontrols
;;; im
;;; cd
