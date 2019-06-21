(defpackage #:iup-controls
  (:use #:common-lisp
        #:serapeum)
  (:export #:open)
  (:shadow #:open))

(in-package #:iup-controls)

(defalias open #'iup-controls-cffi::%iup-controls-open)

(iup::defiupclasses "IUP-CONTROLS")
