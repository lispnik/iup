(defpackage #:iup-olecontrol-cffi
  (:use #:common-lisp
	#:cffi))

(defpackage #:iup-olecontrol
  (:use #:common-lisp)
  (:export #:open
	   #:olecontrol)
  (:import-from #:iup-utils
		#:alias)
  (:shadow #:open))
