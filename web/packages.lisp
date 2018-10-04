(defpackage #:iup-web-cffi
  (:use #:common-lisp
	#:cffi))

(defpackage #:iup-web
  (:use #:common-lisp)
  (:export #:web-browser-open
	   #:web-browser))
