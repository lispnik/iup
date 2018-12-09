(defpackage #:iup-web
  (:use #:common-lisp)
  (:export #:open)
  (:import-from #:serapeum #:defalias)
  (:shadow #:open))

(in-package #:iup-web)

(defalias open #'iup-web-cffi::%iup-web-browser-open)

(iup::defiupclasses "IUP-WEB")
