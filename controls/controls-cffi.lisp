(in-package #:iup-controls-cffi)

(define-foreign-library iup-controls
  (:unix "libiupcontrols.so")
  (:windows "iupcontrols.dll")
  (t (:default "iupcontrols")))

(use-foreign-library iup-controls)

(defcfun (%iup-controls-open "IupControlsOpen") :void)
