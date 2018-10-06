(in-package #:iup-web)

(iup::alias 'web-browser-open #'iup-web-cffi::%iup-web-browser-open)
(iup::defattributefun web-browser () (iup-web-cffi::%iup-web-browser))
