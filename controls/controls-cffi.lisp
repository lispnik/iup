(in-package #:iup-controls-cffi)

(define-foreign-library iupcontrols
  (:unix "libiupcontrols.so")
  (t (:default "iupcontrols")))

(use-foreign-library iupcontrols)

(defcfun (iup-controls-cffi::%iup-controls-open "IupControlsOpen") :void)
