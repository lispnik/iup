(defpackage #:iup-mglplot
  (:use #:common-lisp
        #:serapeum)
  (:export #:open)
  (:shadow #:open))

(in-package #:iup-mglplot)

(defalias open #'iup-mglplot-cffi::%iup-mglplot-open)

(iup::defiupclasses "IUP-MGLPLOT")
