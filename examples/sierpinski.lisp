(defpackage #:iup-examples.sierpinksi
  (:use #:common-lisp)
  (:export #:sierpinksi))

(in-package #:iup-examples.sierpinksi)

(defparameter *canvas* nil)

(defun canvas-redraw (handle x y)
  (cd:activate *canvas*)
  (cd:clear *canvas*)
  (setf (cd:foreground *canvas*) cd:+red+)
  (sierpinski-draw *canvas* *levels*)
  (cd:flush *canvas*)
  iup:+default+)

(defun canvas-map (handle)
  (setf *canvas* (cd:create-canvas (iup-cd:context-iup-dbuffer) handle))
  iup:+default+)

(defun canvas-unmap (handle)
  (cd:kill *canvas*)
  iup:+default+)
