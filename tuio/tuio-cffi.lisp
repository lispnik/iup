(defpackage #:iup-tuio-cffi
  (:use #:common-lisp))

(in-package #:iup-tuio-cffi)

(cffi:define-foreign-library iup-tuio
  (:unix "libiuptuio.so")
  (:windows "iuptuio.dll")
  (t (:default "iuptuio")))

(cffi:use-foreign-library iup-tuio)

(cffi:defcfun (%iup-tuio-open "IupTuioOpen") :int)
