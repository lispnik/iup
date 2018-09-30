#-:asdf3.1 (error "IUP requires ASDF 3")

(in-package #:asdf-user)

(defsystem #:iup
  :description "Lua-like CFFI bindings to IUP Portable User Interface library"
  :author "Matthew Kennedy <burnsidemk@gmail.com>"
  :licence "LLGPL"
  :version (:read-file-line "version.txt")
  :components
  ((:module "src"
    :serial t
    :components ((:file "package")
		 (:file "iup-cffi")
		 (:file "iup"))))
  :in-order-to ((test-op (test-op #:iup-test)))
  :depends-on (#:alexandria
	       #:cffi
	       #:cl-syntax
	       #:cl-syntax-annot))
