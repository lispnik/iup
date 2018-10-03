#-asdf3.2 (error "IUP requires ASDF 3")

(defsystem #:iup
  :description "CFFI bindings to the IUP Portable User Interface library"
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :pathname "src/"
  :components ((:file "iup-cffi")
	       (:file "iup"))
  :depends-on (#:alexandria
	       #:cffi))

(defsystem #:iup/plot
  :serial t
  :pathname "srcplot/"
  :components ((:file "plot-cffi")
	       (:file "plot"))
  :depends-on (#:alexandria
	       #:cffi
	       #:iup))

(defsystem #:iup/mglplot
  :serial t
  :pathname "srcmglplot/"
  :components ((:file "mglplot-cffi")
	       (:file "mglplot"))
  :depends-on (#:alexandria
	       #:cffi
	       #:iup))

(defsystem #:iup/web
  :serial t
  :pathname "srcweb/"
  :components ((:file "web-cffi")
	       (:file "web"))
  :depends-on (#:alexandria
	       #:cffi
	       #:iup))

(defsystem #:iup/scintilla
  :serial t
  :pathname "srcscintilla/"
  :components ((:file "scintilla-cffi")
	       (:file "scintilla"))
  :depends-on (#:alexandria
	       #:cffi
	       #:iup))


;;; gl
;;; glcontrols
;;; im
;;; cd
