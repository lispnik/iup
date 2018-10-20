(defpackage #:iup-mglplot-cffi
  (:use #:common-lisp
	#:cffi))

(defpackage #:iup-mglplot
  (:use #:common-lisp)
  (:export #:open)
  (:import-from #:iup-utils
		#:alias)
  (:shadow #:open))
