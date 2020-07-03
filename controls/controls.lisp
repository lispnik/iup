(defpackage #:iup-controls
  (:use #:common-lisp)
  (:export #:open)
  (:import-from #:tecgraf-base
                #:defalias)
  (:shadow #:open))

(in-package #:iup-controls)

(defalias open #'iup-controls-cffi::%iup-controls-open)

(iup::defiupclasses "IUP-CONTROLS")
