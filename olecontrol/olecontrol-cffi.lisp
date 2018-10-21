(in-package #:iup-olecontrol-cffi)

(define-foreign-library iup-olecontrol
  (:windows "iupole.dll")
  (t (:default "iupole")))

(use-foreign-library iup-olecontrol)

(defcfun (%iup-ole-control-open "IupOleControlOpen") :int)
