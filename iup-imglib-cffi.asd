(in-package #:asdf-user)

(defsystem #:iup-imglib-cffi
  :pathname "imglib-cffi/"
  :components ((:file "imglib-cffi"))
  :depends-on (#:iup-cffi
	       #:cffi))
