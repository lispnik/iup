(in-package #:iup-web-cffi)

(define-foreign-library iupweb
  (:unix "libiupweb.so")
  (t (:default "iupweb")))

(use-foreign-library iupweb)

(defcfun (%iup-web-browser-open "IupWebBrowserOpen") :int)
(defcfun (%iup-web-browser "IupWebBrowser") iup-cffi::ihandle)
