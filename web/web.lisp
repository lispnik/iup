(defpackage #:iup-web
  (:use #:common-lisp)
  (:export #:open
	   #:web-browser)
  (:import-from #:iup-utils
		#:alias)
  (:shadow #:open))

(in-package #:iup-web)

(alias 'open #'iup-web-cffi::%iup-web-browser-open)
