(defpackage #:iup-olecontrol
  (:use #:common-lisp)
  (:export #:open
	   #:olecontrol)
  (:import-from #:tecgraf-base
                #:defalias)
  (:shadow #:open))

(in-package #:iup-olecontrol)

(defalias open #'iup-olecontrol-cffi::%iup-olecontrol-open)

(iup::defiupclasses "IUP-OLECONTROL")
