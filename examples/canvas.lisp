(defpackage #:iup-examples.canvas
  (:use #:common-lisp
	#:alexandria)
  (:export #:canvas))

(in-package #:iup-examples.canvas)

(defparameter *levels* 0)
(defparameter *canvas* nil)

(defun canvas-spin (handle pos)
  (declare (ignore pos))
  (setf *levels* (iup:attribute handle "VALUE" 'integer))
  ;; FIXME trigger redraw here
  iup:+default+)

(defun canvas-redraw (handle x y)
  (declare (ignore handle x y))
  (cd:activate *canvas*)
  (cd:clear *canvas*)
  (setf (cd:foreground *canvas*) cd:+red+)
  (cd-examples.sierpinski:sierpinski *canvas* *levels*)
  (cd:flush *canvas*)
  iup:+default+)

(defun canvas-map (handle)
  (setf *canvas* (cd:create-canvas (iup-cd:context-iup-dbuffer) handle))
  iup:+default+)

(defun canvas-unmap (handle)
  (declare (ignore handle))
  (cd:kill *canvas*)
  iup:+default+)

(defun sierpinski ()
  (iup:with-iup ()
    (let* ((canvas (iup:canvas :rastersize "200x200"))
	   (spin (iup:text :spin "YES" :spinmin 0 :spinmax 5))
	   (vbox (iup:vbox (list canvas spin) :alignment "ACENTER"))
	   (dialog (iup:dialog vbox)))
      (setf (iup:callback canvas :map_cb) 'canvas-map
	    (iup:callback canvas :unmap_cb) 'canvas-unmap
	    (iup:callback canvas :action) 'canvas-redraw
	    (iup:callback spin :spin_cb) 'canvas-spin
	    *levels* 0)
      (iup:show-xy dialog iup:+center+ iup:+center+)
      (iup:main-loop))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (sierpinski))
