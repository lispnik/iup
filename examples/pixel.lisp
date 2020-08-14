(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-cd" "cd")))

(defpackage #:iup-examples.pixel
  (:use #:common-lisp)
  (:export #:pixel))

(in-package #:iup-examples.pixel)

(defparameter *canvas* nil)

(defun canvas-redraw (handle x y)
  (declare (ignore handle x y))
  (cd:activate *canvas*)
  (setf (cd:background *canvas*) cd:+white+)
  (cd:clear *canvas*)
  (multiple-value-bind (w h)
      (cd:size *canvas*)
    (cd:pixel *canvas* (/ w 2) (/ h 2) cd:+black+))
  (cd:flush *canvas*)
  iup:+default+)

(defun canvas-map (handle)
  (setf *canvas* (cd:create-canvas (iup-cd:context-iup-dbuffer) handle))
  iup:+default+)

(defun canvas-unmap (handle)
  (cd:kill *canvas*)
  (setf *canvas* nil)
  iup:+default+)

(defun canvas ()
  (iup:with-iup ()
    (let* ((canvas
             (iup:canvas :rastersize "320x200"
                         :map_cb 'canvas-map
                         :unmap_cb 'canvas-unmap
                         :action 'canvas-redraw))
	   (dialog
             (iup:dialog canvas :title "Pixel example")))
      (iup:show dialog)
      (iup:main-loop))))

#-sbcl (canvas)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (canvas))
