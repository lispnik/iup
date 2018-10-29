(defpackage #:iup-imglib
  (:use #:common-lisp
	#:iup-utils)
  (:export #:open)
  (:shadow #:open))

(in-package #:iup-imglib)

(alias 'open #'iup-imglib-cffi::%iup-imglib-open)
