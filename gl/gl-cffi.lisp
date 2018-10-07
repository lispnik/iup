(in-package #:iup-gl-cffi)

(define-foreign-library iupgl
  (:unix "libiupgl.so")
  (t (:default "iupgl")))

(use-foreign-library iupgl)

(defcfun (%iup-gl-canvas-open "IupGLCanvasOpen") :void)

(defcfun (%iup-gl-canvas "IupGLCanvas") iup-cffi::ihandle
  (action :string))

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
