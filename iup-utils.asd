(in-package #:asdf-user)

(defsystem #:iup-utils
  :serial t
  :pathname "utils/"
  :components ((:file "utils"))
  :depends-on (#:alexandria
	       #:cffi))

