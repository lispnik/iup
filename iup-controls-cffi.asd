(in-package #:asdf-user)

(defsystem #:iup-controls-cffi
  :pathname "controls-cffi/"
  :components ((:file "controls-cffi"))
  :depends-on (#:cffi
	       #:iup-cffi))
