(in-package #:iup-glcontrols-cffi)

(define-foreign-library iup-glcontrols
  (:unix "libiupglcontrols.so")
  (:windows "iupglcontrols.dll")
  (t (:default "iupglcontrols")))

(use-foreign-library iup-glcontrols)

(defcfun (%iup-glcontrols-open "IupGLControlsOpen") :int)

(defcfun (%iup-glcontrols-draw-image "IupGLDrawImage") :void
  (handle iup-cffi::ihandle)
  (name :string)
  (x :int)
  (y :int)
  (active-p :boolean))

(defcfun (%iup-glcontrols-draw-text "IupGLDrawText") :void
  (handle iup-cffi::ihandle)
  (string :string)
  (length :int)
  (x :int)
  (y :int))

(defcfun (%iup-glcontrols-draw-get-text-size "IupGLDrawGetTextSize") :void
  (handle iup-cffi::ihandle)
  (string :string)
  (length :int)
  (x (:pointer :int))
  (y (:pointer :int)))

(defcfun (%iup-glcontrols-draw-get-image-info "IupGLDrawGetImageInfo") :void
  (name :string)
  (w (:pointer :int))
  (h (:pointer :int))
  (bpp (:pointer :int)))
