(defpackage #:iup-classesdb-cffi
  (:use #:common-lisp))

(uiop:define-package #:iup-classesdb-cffi.accessors)
(uiop:define-package #:iup-classesdb-cffi.functions)

(defpackage #:iup-classesdb
  (:use #:common-lisp
	#:alexandria)
  (:export #:regenerate))
