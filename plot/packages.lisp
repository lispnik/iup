(defpackage #:iup-plot-cffi
  (:use #:common-lisp
	#:cffi))

(defpackage #:iup-plot
  (:use #:common-lisp)
  (:export #:open)
  (:import-from #:iup-utils
		#:alias)
  (:shadow #:open))
