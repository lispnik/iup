(in-package #:iup-web)

(iup::alias 'web-browser-open #'iup-web-cffi::%iup-web-browser-open)
(iup::defattrfun web-browser () (iup-web-cffi::%iup-web-browser))
