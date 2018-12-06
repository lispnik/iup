(defpackage #:iup-olecontrol
  (:use #:common-lisp)
  (:export #:open
	   #:olecontrol)
  (:import-from #:iup-utils
		#:alias)
  (:shadow #:open))

(in-package #:iup-olecontrol)

(alias 'open #'iup-olecontrol-cffi::%iup-olecontrol-open)

(iup::defiupclasses "IUP-OLECONTROL")
