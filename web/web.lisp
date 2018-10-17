(in-package #:iup-web)

(alias 'open #'iup-web-cffi::%iup-web-browser-open)
(iup::defattributefun web-browser () (iup-web-cffi::%iup-web-browser))
