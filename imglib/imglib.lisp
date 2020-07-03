(defpackage #:iup-imglib
  (:use #:common-lisp)
  (:export #:open)
  (:import-from #:tecgraf-base
                #:defalias)
  (:shadow #:open))

(in-package #:iup-imglib)

(defalias open #'iup-imglib-cffi::%iup-imglib-open)
