(defpackage #:iup-glcontrols-cffi
  (:use #:common-lisp
	#:iup-cffi))

(in-package #:iup-glcontrols-cffi)

(cffi:define-foreign-library iup-glcontrols
  (:unix "libiupglcontrols.so")
  (:windows "iupglcontrols.dll")
  (t (:default "iupglcontrols")))

(cffi:use-foreign-library iup-glcontrols)

(cffi:defcfun (%iup-glcontrols-open "IupGLControlsOpen") :int)

(cffi:defcfun (%iup-glcontrols-draw-image "IupGLDrawImage") :void
  (handle ihandle)
  (name :string)
  (x :int)
  (y :int)
  (active-p :boolean))

(cffi:defcfun (%iup-glcontrols-draw-text "IupGLDrawText") :void
  (handle ihandle)
  (string :string)
  (length :int)
  (x :int)
  (y :int))

(cffi:defcfun (%iup-glcontrols-draw-get-text-size "IupGLDrawGetTextSize") :void
  (handle ihandle)
  (string :string)
  (length :int)
  (x (:pointer :int))
  (y (:pointer :int)))

(cffi:defcfun (%iup-glcontrols-draw-get-image-info "IupGLDrawGetImageInfo") :void
  (name :string)
  (w (:pointer :int))
  (h (:pointer :int))
  (bpp (:pointer :int)))
