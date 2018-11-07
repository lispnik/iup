(defpackage #:iup-imglib-cffi
  (:use #:common-lisp))

(in-package #:iup-imglib-cffi)

(cffi:define-foreign-library iup-imglib
  (:unix "libiupimglib.so")
  (:windows "iupimglib.dll")
  (t (:default "iupimglib")))

(cffi:use-foreign-library iup-imglib)

(cffi:defcfun (%iup-image-lib-open "IupImageLibOpen") :void)

