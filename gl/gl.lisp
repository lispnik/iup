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

(in-package #:iup-gl)

(alias 'open #'iup-gl-cffi::%iup-gl-canvas-open)

(iup::defiupclasses "IUP-GL")

(alias 'make-current #'iup-gl-cffi::%iup-gl-make-current)
(alias 'current-p #'iup-gl-cffi::%iup-gl-is-current)
(alias 'swap-buffers #'iup-gl-cffi::%iup-gl-swap-buffers)
(alias 'palette #'iup-gl-cffi::%iup-gl-palette)
(alias 'use-font #'iup-gl-cffi::%iup-gl-use-font)
(alias 'wait #'iup-gl-cffi::%iup-gl-wait)
