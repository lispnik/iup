(defpackage #:iup-olecontrol
  (:use #:common-lisp
        #:serapeum)
  (:export #:open
	   #:olecontrol)
  (:shadow #:open))

(in-package #:iup-olecontrol)

(defalias open #'iup-olecontrol-cffi::%iup-olecontrol-open)

(iup::defiupclasses "IUP-OLECONTROL")
