(defpackage #:iup-plot-cffi
  (:use #:common-lisp
	#:cffi))

(defpackage #:iup-plot
  (:use #:common-lisp)
  (:export)
  (:import-from #:iup-utils
		#:alias))
