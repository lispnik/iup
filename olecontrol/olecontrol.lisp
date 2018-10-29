(defpackage #:iup-olecontrol
  (:use #:common-lisp)
  (:export #:open
	   #:olecontrol)
  (:import-from #:iup-utils
		#:alias)
  (:shadow #:open))

(in-package #:iup-olecontrol)

(alias 'open #'iup-olecontrol-cffi::%iup-ole-control-open)
