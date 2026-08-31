(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup/cd" "cd")))

(defpackage #:iup-example.ltk-eyes
  (:use #:common-lisp)
  (:export #:ltk-eyes))

(in-package #:iup-example.ltk-eyes)

(defvar *canvas* nil)

(defun canvas-map (handle)
  (setf *canvas* (cd:make-canvas (iup-cd:context-iup-dbuffer) handle))
  iup:+default+)

(defun canvas-unmap (handle)
  (declare (ignore handle))
  (when *canvas*
    (cd:kill *canvas*)
    (setf *canvas* nil))
  iup:+default+)

(defun canvas-redraw (handle x y)
  (declare (ignore handle x y))
  (draw *canvas*)
  iup:+default+)

(defun draw (canvas)
  (cd:activate canvas)
  (setf (cd:foreground canvas) cd:+black+
	(cd:background canvas) cd:+white+
	(cd:line-width canvas) 10)
  (cd:clear canvas)
  (multiple-value-bind (w h)
      (cd:canvas-size canvas)
    (let* ((xc1 (* w 1/4))
	   (yc1 (* h 1/2))
	   (w1 (* w 1/3))
	   (h1 (* h 2/3))
	   (xc2 (* w 3/4))
	   (yc2 yc1)
	   (w2 w1)
	   (h2 h1)
	   (a1 0)
	   (a2 360))
      (cd:arc canvas xc1 yc1 w1 h1 a1 a2)
      (cd:arc canvas xc2 yc2 w2 h2 a1 a2)
      (cd:sector canvas xc1 yc1 (* w1 1/4) (* h1 1/4) 0 360)
      (cd:sector canvas xc2 yc2 (* w1 1/4) (* h1 1/4) 0 360)))
  (cd:flush canvas)
  iup:+default+)

(defun ltk-eyes ()
  (iup:with-iup ()
    (let* ((canvas (iup:canvas :map_cb 'canvas-map
			       :unmap_cb 'canvas-unmap
			       :action 'canvas-redraw))
	   (dialog (iup:dialog canvas :title "IUP LTK Eyes"
				      :size "500x320")))
      (iup:show-xy dialog iup:+center+ iup:+center+)
      (iup:main-loop))))

;;; TODO the pupils should track the mouse, as in the original LTK demo this
;;; was transcribed from.

#-sbcl (ltk-eyes)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (ltk-eyes))
