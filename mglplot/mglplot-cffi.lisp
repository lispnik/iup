(in-package #:iup-mglplot-cffi)

(define-foreign-library iup-mglplot
  (:unix "libiup_mglplot.so")
  (:windows "iup_mglplot.dll")
  (t (:default "iup_mglplot")))

(use-foreign-library iup-mglplot)

(defcfun (%iup-mglplot-open "IupMglPlotOpen") :void)

