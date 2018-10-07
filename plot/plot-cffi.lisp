(in-package #:iup-plot-cffi)

(define-foreign-library iupcontrols
  (:unix "libiupcontrols.so")
  (t (:default "iupcontrols")))

(use-foreign-library iupcontrols)
