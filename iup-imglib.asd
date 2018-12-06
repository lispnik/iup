(defsystem #:iup-imglib
  :serial t
  :pathname "imglib"
  :components ((:file "imglib"))
  :depends-on (#:iup-imglib-cffi
	       #:iup
	       #:iup-utils))
