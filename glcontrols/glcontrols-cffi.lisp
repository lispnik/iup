(in-package #:iup-glcontrols-cffi)

(define-foreign-library iupglcontrols
  (:unix "libiupglcontrols.so")
  (t (:default "iupglcontrols")))

(use-foreign-library iupglcontrols)

(defcfun (%iup-glcontrols-open "IupGLControlsOpen") :int)

(defcfun (%iup-glcontrols-canvas-box "IupGLCanvasBoxv") iup-cffi::ihandle
  (children :pointer))

(defcfun (%iup-glcontrols-sub-canvas "IupGLSubCanvas") iup-cffi::ihandle)
(defcfun (%iup-glcontrols-separator "IupGLSeparator") iup-cffi::ihandle)
(defcfun (%iup-glcontrols-progress-bar "IupGLProgressBar") iup-cffi::ihandle)
(defcfun (%iup-glcontrols-text "IupGLText") iup-cffi::ihandle)

(defcfun (%iup-glcontrols-val "IupGLVal") iup-cffi::ihandle
  (orientation :string))

(defcfun (%iup-glcontrols-label "IupGLLabel") iup-cffi::ihandle
  (title :string))

(defcfun (%iup-glcontrols-button "IupGLButton") iup-cffi::ihandle
  (title :string))

(defcfun (%iup-glcontrols-toggle "IupGLToggle") iup-cffi::ihandle
  (title :string))

(defcfun (%iup-glcontrols-link "IupGLLink") iup-cffi::ihandle
  (url :string)
  (title :string))

(defcfun (%iup-glcontrols-frame "IupGLFrame") iup-cffi::ihandle (child iup-cffi::ihandle))
(defcfun (%iup-glcontrols-expander "IupGLExpander") iup-cffi::ihandle (child iup-cffi::ihandle))
(defcfun (%iup-glcontrols-scroll-box "IupGLScrollBox") iup-cffi::ihandle (child iup-cffi::ihandle))
(defcfun (%iup-glcontrols-size-box "IupGLSizeBox") iup-cffi::ihandle (child iup-cffi::ihandle))

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
