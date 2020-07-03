(defpackage #:iup-tuio
  (:use #:common-lisp)
  (:export #:open)
  (:shadow #:open))

(in-package #:iup-tuio)

(setf (fdefinition 'open) #'iup-tuio-cffi::%iup-tuio-open)

(iup::defiupclasses "IUP-TUIO")
