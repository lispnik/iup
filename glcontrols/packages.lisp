(defpackage #:iup-glcontrols-cffi
  (:use #:common-lisp
	#:cffi))

(defpackage #:iup-glcontrols
  (:use #:common-lisp)
  (:export #:open
	   #:draw-image
	   #:draw-text
	   #:draw-get-text-size
	   #:draw-get-image-info)
  (:import-from #:iup-utils
		#:alias)
  (:shadow #:open))
