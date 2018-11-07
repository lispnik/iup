(defpackage #:iup-web-cffi (:use #:common-lisp))
(in-package #:iup-web-cffi)

(cffi:define-foreign-library iup-web
  (:unix "libiupweb.so")
  (:windows "iupweb.dll")
  (t (:default "iupweb")))

(cffi:use-foreign-library iup-web)

(cffi:defcfun (%iup-web-browser-open "IupWebBrowserOpen") :int)
