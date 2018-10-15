(defpackage #:iup-glcontrols-cffi
  (:use #:common-lisp
	#:cffi))

(defpackage #:iup-glcontrols
  (:use #:common-lisp)
  (:export #:open
	   #:canvas-box
	   #:sub-canvas
	   #:label
	   #:separator
	   #:button
	   #:toggle
	   #:link
	   #:progress-bar
	   #:val
	   #:frame
	   #:expander
	   #:scroll-box
	   #:size-box
	   #:text
	   #:draw-image
	   #:draw-text
	   #:draw-get-text-size
	   #:draw-get-image-info)
  (:import-from #:iup-utils
		#:alias)
  (:shadow #:open))
