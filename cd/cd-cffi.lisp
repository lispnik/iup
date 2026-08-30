(defpackage #:iup-cd-cffi
  (:use #:common-lisp))

(in-package #:iup-cd-cffi)

(cffi:define-foreign-library iup-cd
  (:darwin (:framework "iupcd"))
  (:unix "libiupcd.so")
  (:windows "iupcd.dll")
  (t (:default "iupcd")))

(cffi:use-foreign-library iup-cd)

(cffi:defcfun (%cd-context-iup "cdContextIup") cd.ffi::cd-context)
(cffi:defcfun (%cd-context-iup-dbuffer "cdContextIupDBuffer") cd.ffi::cd-context)
(cffi:defcfun (%cd-context-iup-dbuffer-rgb "cdContextIupDBufferRGB") cd.ffi::cd-context)
(cffi:defcfun (%cd-context-iup-draw "cdContextIupDraw") cd.ffi::cd-context)
