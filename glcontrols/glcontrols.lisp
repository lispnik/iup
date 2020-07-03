(defpackage #:iup-glcontrols
  (:use #:common-lisp)
  (:export #:open
	   #:draw-image
	   #:draw-text
	   #:draw-get-text-size
	   #:draw-get-image-info)
  (:import-from #:tecgraf-base
                #:defalias)
  (:shadow #:open))

(in-package #:iup-glcontrols)

(defalias open #'iup-glcontrols-cffi::%iup-glcontrols-open)

(iup::defiupclasses "IUP-GLCONTROLS")

(defun draw-text (handle text x y)
  (iup-glcontrols-cffi::%iup-glcontrols-draw-text handle text -1 x y))

(defalias draw-image #'iup-glcontrols-cffi::%iup-glcontrols-draw-image)

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
