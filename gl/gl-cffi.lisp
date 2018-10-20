(in-package #:iup-gl-cffi)

(define-foreign-library iup-gl
  (:unix "libiupgl.so")
  (:windows "iupgl.dll")
  (t (:default "iupgl")))

(use-foreign-library iup-gl)

(defcfun (%iup-gl-canvas-open "IupGLCanvasOpen") :void)

(defcfun (%iup-gl-make-current "IupGLMakeCurrent") :void
  (handle iup-cffi::ihandle))

(defcfun (%iup-gl-is-current "IupGLIsCurrent") :boolean
  (handle iup-cffi::ihandle))

(defcfun (%iup-gl-swap-buffers "IupGLSwapBuffers") :void
  (handle iup-cffi::ihandle))

(defcfun (%iup-gl-palette "IupGLPalette") :void
  (handle iup-cffi::ihandle)
  (index :int)
  (r :float)
  (g :float)
  (b :float))

(defcfun (%iup-gl-use-font "IupGLUseFont") :void
  (handle iup-cffi::ihandle)
  (first :int)
  (count :int)
  (list-base :int))

(defcfun (%iup-gl-wait "IupGLWait") :void
  (gl :int))
