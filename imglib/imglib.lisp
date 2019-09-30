(defpackage #:iup-imglib
  (:use #:common-lisp
        #:serapeum)
  (:export #:open)
  (:shadow #:open))

(in-package #:iup-imglib)

(defalias open #'iup-imglib-cffi::%iup-imglib-open)
