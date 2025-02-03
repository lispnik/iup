(defpackage #:iup-gl-cffi
  (:use #:common-lisp)
  (:import-from #:iup-cffi #:ihandle))

(in-package #:iup-gl-cffi)

(cffi:define-foreign-library iup-gl
  (:unix "libiupgl.so")
  (:windows "iupgl.dll")
  (t (:default "iupgl")))

(cffi:use-foreign-library iup-gl)

(cffi:defcfun (%iup-gl-canvas-open "IupGLCanvasOpen") :void)

(cffi:defcfun (%iup-gl-make-current "IupGLMakeCurrent") :void
  (handle ihandle))

(cffi:defcfun (%iup-gl-is-current "IupGLIsCurrent") :boolean
  (handle ihandle))

(cffi:defcfun (%iup-gl-swap-buffers "IupGLSwapBuffers") :void
  (handle ihandle))

(cffi:defcfun (%iup-gl-palette "IupGLPalette") :void
  (handle ihandle)
  (index :int)
  (r :float)
  (g :float)
  (b :float))

(cffi:defcfun (%iup-gl-use-font "IupGLUseFont") :void
  (handle ihandle)
  (first :int)
  (count :int)
  (list-base :int))

(cffi:defcfun (%iup-gl-wait "IupGLWait") :void
  (gl :int))
