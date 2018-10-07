(defpackage #:iup-gl-cffi
  (:use #:common-lisp
	#:cffi))

(defpackage #:iup-gl
  (:use #:common-lisp)
  (:export #:open
	   #:canvas
	   #:background-box
	   #:make-current
	   #:current-p
	   #:swap-buffers
	   #:palette
	   #:use-font
	   #:wait)
  (:import-from #:iup-utils
		#:alias)
  (:shadow #:open))
