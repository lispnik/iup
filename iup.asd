(defsystem #:iup
  :description "CFFI bindings to the IUP Portable User Interface library"
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :homepage "https://github.com/lispnik/iup"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :pathname "iup/"
  :components ((:file "packages")
	       (:file "callback")
	       (:file "classes")
	       (:file "iup")
	       (:file "config"))
  :depends-on (#:iup-cffi
	       #:iup-utils
	       #:cffi
	       #:alexandria
	       #:genhash
	       #:trivial-arguments))
