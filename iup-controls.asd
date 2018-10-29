(in-package #:asdf-user)

(defsystem #:iup-controls
  :serial t
  :pathname "controls/"
  :components ((:file "controls"))
  :depends-on (#:iup
	       #:iup-controls-cffi
	       #:iup-utils))

