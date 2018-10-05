(in-package #:iup-cffi)

(define-foreign-library iup
  (:unix "libiup.so")
  (t (:default "iup")))

(define-foreign-library iupimglib
  (:unix "libiupimglib.so")
  (t (:default "iupimglib")))

(use-foreign-library iup)
(use-foreign-library iupimglib)
