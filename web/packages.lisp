(defpackage #:iup-web-cffi
  (:use #:common-lisp
	#:cffi))

(defpackage #:iup-web
  (:use #:common-lisp)
  (:export #:open
	   #:web-browser)
  (:import-from #:iup-utils
		#:alias)
  (:shadow #:open))
