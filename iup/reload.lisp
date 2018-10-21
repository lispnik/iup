(in-package #:iup-cffi)

(define-foreign-library iup
  (:unix "libiup.so")
  (:windows "iup.dll")
  (t (:default "iup")))

(define-foreign-library iup-imglib
  (:unix "libiupimglib.so")
  (:windows "iupimglib.dll")
  (t (:default "iupimglib")))

(use-foreign-library iup)
(use-foreign-library iup-imglib)
