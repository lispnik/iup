(defpackage #:iup-mglplot
  (:use #:common-lisp)
  (:export #:open)
  (:import-from #:iup-utils
		#:alias)
  (:shadow #:open))

(in-package #:iup-mglplot)

(alias 'open #'iup-mglplot-cffi::%iup-mglplot-open)
