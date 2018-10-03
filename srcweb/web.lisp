(defpackage #:iup-web
  (:use #:common-lisp
	#:iup)
  (:export #:web-browser-open
	   #:web-browser)
  (:shadow #:open
	   #:close))

(in-package #:iup-web)

(iup::alias 'web-browser-open #'iup-cffi::%iup-web-browser-open)

(defun web-browser (&rest attrs &key &allow-other-keys)
  (iup::apply-attrs (iup-cffi::%iup-web-browser) attrs))
