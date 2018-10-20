(in-package #:iup-web-cffi)

(define-foreign-library iup-web
  (:unix "libiupweb.so")
  (:windows "iupweb.dll")
  (t (:default "iupweb")))

(use-foreign-library iup-web)

(defcfun (%iup-web-browser-open "IupWebBrowserOpen") :int)
