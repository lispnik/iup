(defpackage #:iup-controls-cffi (:use #:common-lisp))
(in-package #:iup-controls-cffi)

(cffi:define-foreign-library iup-controls
  (:unix "libiupcontrols.so")
  (:windows "iupcontrols.dll")
  (t (:default "iupcontrols")))

(cffi:use-foreign-library iup-controls)

(cffi:defcfun (%iup-controls-open "IupControlsOpen") :void)
