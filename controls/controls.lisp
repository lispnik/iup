(defpackage #:iup-controls
  (:use #:common-lisp
	#:cffi
	#:iup-utils)
  (:export #:open)
  (:shadow #:open))

(in-package #:iup-controls)

(alias 'open #'iup-controls-cffi::%iup-controls-open)

(iup::defiupclasses "IUP-CONTROLS")
