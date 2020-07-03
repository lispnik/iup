(defpackage #:iup-mglplot
  (:use #:common-lisp)
  (:export #:open)
  (:import-from #:tecgraf-base
                #:defalias)
  (:shadow #:open))

(in-package #:iup-mglplot)

(defalias open #'iup-mglplot-cffi::%iup-mglplot-open)

(iup::defiupclasses "IUP-MGLPLOT")
