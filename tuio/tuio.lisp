(defpackage #:iup-tuio
  (:use #:common-lisp
        #:serapeum)
  (:export #:open)
  (:shadow #:open))

(in-package #:iup-tuio)

(defalias open #'iup-tuio-cffi::%iup-tuio-open)

(iup::defiupclasses "IUP-TUIO")
