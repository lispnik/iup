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
  (:import-from #:tecgraf-base
                #:defalias)
  (:shadow #:open))

(in-package #:iup-gl)

(defalias open #'iup-gl-cffi::%iup-gl-canvas-open)

(iup::defiupclasses "IUP-GL")

(defalias make-current #'iup-gl-cffi::%iup-gl-make-current)
(defalias current-p #'iup-gl-cffi::%iup-gl-is-current)
(defalias swap-buffers #'iup-gl-cffi::%iup-gl-swap-buffers)
(defalias palette #'iup-gl-cffi::%iup-gl-palette)
(defalias use-font #'iup-gl-cffi::%iup-gl-use-font)
(defalias wait #'iup-gl-cffi::%iup-gl-wait)
