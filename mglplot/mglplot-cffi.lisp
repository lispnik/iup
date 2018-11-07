(defpackage #:iup-mglplot-cffi
  (:use #:common-lisp))

(in-package #:iup-mglplot-cffi)

(cffi:define-foreign-library iup-mglplot
  (:unix "libiup_mglplot.so")
  (:windows "iup_mglplot.dll")
  (t (:default "iup_mglplot")))

(cffi:use-foreign-library iup-mglplot)

(cffi:defcfun (%iup-mglplot-open "IupMglPlotOpen") :void)

