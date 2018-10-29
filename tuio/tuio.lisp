(defpackage #:iup-tuio
  (:use #:common-lisp)
  (:export #:open)
  (:import-from #:iup-utils
		#:alias)
  (:shadow #:open))

(in-package #:iup-tuio)

(alias 'open #'iup-tuio-cffi::%iup-tuio--open)
