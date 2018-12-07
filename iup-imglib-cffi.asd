(defsystem #:iup-imglib-cffi
  :pathname "imglib"
  :components ((:file "imglib-cffi"))
  :depends-on (#:iup-cffi
	       #:cffi))
