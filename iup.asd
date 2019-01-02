(defsystem #:iup
  :description "CFFI bindings to the IUP Portable User Interface library"
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :homepage "https://github.com/lispnik/iup"
  :licence "MIT"
  :version (:read-file-line "version.txt")
  :serial t
  :pathname "iup"
  :components ((:file "packages")
	       (:file "constants")
	       (:file "callback")
	       (:file "classes")
	       (:file "config")
	       (:file "attributes")
	       (:file "iup"))
  :depends-on (#:iup-cffi
	       #:iup-utils
	       #:cffi
	       #:alexandria
	       #:serapeum
	       #:genhash
	       #:trivial-arguments
	       #:split-sequence))
