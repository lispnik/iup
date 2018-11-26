(defpackage #:iup-examples.canvas
  (:use #:common-lisp
	#:alexandria)
  (:export #:canvas))

(in-package #:iup-examples.canvas)

(defparameter *levels* 0)
(defparameter *canvas* nil)

(defun sierpinski-spin (handle pos)
  (declare (ignore pos))
  (setf *levels* (iup:attribute handle "VALUE" 'integer))
  iup:+default+)

(defun sierpinski-redraw (handle x y)
  (declare (ignore handle x y))
  (cd:activate *canvas*)
  (cd:clear *canvas*)
  (setf (cd:foreground *canvas*) cd:+red+)
  (cd-examples.sierpinski:sierpinski *canvas* *levels*)
  iup:+default+)

(defun sierpinski-map (handle)
  (setf *canvas* (cd:create-canvas (iup-cd:context-iup) handle))
  iup:+default+)

(defun sierpinski ()
  (iup:with-iup ()
    (let* ((canvas (iup:canvas :rastersize "200x200"))
	   (spin (iup:text :spin "YES" :spinmin 0 :spinmax 5))
	   (vbox (iup:vbox (list canvas spin) :alignment "ACENTER"))
	   (dialog (iup:dialog vbox)))
      (setf (iup:callback canvas :map_cb) 'sierpinski-map
	    (iup:callback canvas :action) 'sierpinski-redraw
	    (iup:callback spin :spin_cb) 'sierpinski-spin)
      (cd-context-plus:initialize)
      (cd:use-context-plus t)
      (iup:show-xy dialog iup:+center+ iup:+center+)
      (iup:main-loop))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (sierpinski))




