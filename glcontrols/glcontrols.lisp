(in-package #:iup-glcontrols)

(alias 'open #'iup-glcontrols-cffi::%iup-glcontrols-open)

(iup::defattributefun-children canvas-box iup-glcontrols-cffi::%iup-glcontrols-canvas-box)

(iup::defattributefun sub-canvas () (iup-glcontrols-cffi::%iup-glcontrols-sub-canvas))
(iup::defattributefun separator () (iup-glcontrols-cffi::%iup-glcontrols-separator))
(iup::defattributefun progress-bar () (iup-glcontrols-cffi::%iup-glcontrols-progress-bar))
(iup::defattributefun val () (iup-glcontrols-cffi::%iup-glcontrols-val nil))
(iup::defattributefun text () (iup-glcontrols-cffi::%iup-glcontrols-text))
(iup::defattributefun label () (iup-glcontrols-cffi::%iup-glcontrols-label nil))
(iup::defattributefun button () (iup-glcontrols-cffi::%iup-glcontrols-label nil))
(iup::defattributefun toggle () (iup-glcontrols-cffi::%iup-glcontrols-toggle nil))
(iup::defattributefun link () (iup-glcontrols-cffi::%iup-glcontrols-link nil nil))
(iup::defattributefun frame (child) (iup-glcontrols-cffi::%iup-glcontrols-frame child))
(iup::defattributefun expander (child) (iup-glcontrols-cffi::%iup-glcontrols-expander child))
(iup::defattributefun scroll-box (child) (iup-glcontrols-cffi::%iup-glcontrols-scroll-box child))
(iup::defattributefun size-box (child) (iup-glcontrols-cffi::%iup-glcontrols-size-box child))

(defun draw-text (handle text x y)
  (iup-glcontrols-cffi::%iup-glcontrols-draw-text handle text -1 x y))

(alias 'draw-image #'iup-glcontrols-cffi::%iup-glcontrols-draw-image)

(defun get-text-size (handle text)
  (cffi:with-foreign-objects
      ((x-ptr :pointer)
       (y-ptr :pointer))
    (iup-glcontrols-cffi::%iup-glcontrols-draw-get-text-size handle text -1 x-ptr y-ptr)
    (values (cffi:mem-ref x-ptr :int)
	    (cffi:mem-ref y-ptr :int))))

(defun get-image-info (name)
    (cffi:with-foreign-objects
      ((w-ptr :pointer)
       (h-ptr :pointer)
       (bpp-ptr :pointer))
    (iup-glcontrols-cffi::%iup-glcontrols-draw-get-image-info name w-ptr h-ptr bpp-ptr)
    (values (cffi:mem-ref w-ptr :int)
	    (cffi:mem-ref h-ptr :int)
	    (cffi:mem-ref bpp-ptr :int))))
