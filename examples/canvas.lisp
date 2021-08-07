(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-cd")))

(defpackage #:iup-examples.canvas
  (:use #:common-lisp
	#:alexandria)
  (:export #:canvas))

(in-package #:iup-examples.canvas)

(defparameter *canvas* nil)

(defun canvas-redraw (handle x y)
  (declare (ignore handle x y))
  (cd:activate *canvas*)
  (multiple-value-bind (w h)
      (cd:size *canvas*)
    (cd:clear *canvas*)
    (let ((stipple (make-array '(100 100) :element-type 'bit)))
      (loop for i below 100
	    do (loop for j below 100
		     do (setf (aref stipple i j) (random 2))))
      (setf (cd:stipple *canvas*) stipple))
    (print (cd:stipple *canvas*))
    (setf (cd:interior-style *canvas*) :interior-stipple
	  (cd:foreground *canvas*) cd:+red+)
    (cd:sector *canvas* (* 1/2 w) (* 1/2 h) (* 1/4 w) (* 1/4 h) 0 360)
    (cd:flush *canvas*))
  iup:+default+)

(defun canvas-map (handle)
  (setf *canvas* (cd:create-canvas (iup-cd:context-iup-dbuffer) handle))
  iup:+default+)

(defun canvas-unmap (handle)
  (cd:kill *canvas*)
  iup:+default+)

(defun canvas ()
  (iup:with-iup ()
    (let* ((canvas (iup:canvas :rastersize "200x200"))
	   (dialog (iup:dialog canvas :title "IUP Canvas")))
      (setf (iup:callback canvas :map_cb) 'canvas-map
	    (iup:callback canvas :unmap_cb) 'canvas-unmap
	    (iup:callback canvas :action) 'canvas-redraw)
      (iup:show-xy dialog iup:+center+ iup:+center+)
      (iup:main-loop))))

#-sbcl (canvas)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (canvas))
