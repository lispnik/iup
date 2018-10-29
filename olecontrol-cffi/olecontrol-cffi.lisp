(defpackage #:iup-olecontrol-cffi (:use #:common-lisp))
(in-package #:iup-olecontrol-cffi)

(cffi:define-foreign-library iup-olecontrol
  (:windows "iupole.dll")
  (t (:default "iupole")))

(cffi:use-foreign-library iup-olecontrol)

(cffi:defcfun (%iup-ole-control-open "IupOleControlOpen") :int)
