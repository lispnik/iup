#-:asdf3.1 (error "IUP requires ASDF 3")

(in-package #:asdf-user)

(defsystem #:iup-plot
  :components
  ((:module "src"
    :serial t
    :components ((:file "iup-plot-cffi")
		 (:file "iup-plot"))))
  :in-order-to ((test-op (test-op #:iup-test)))
  :depends-on (#:alexandria
	       #:cffi
	       #:iup))
