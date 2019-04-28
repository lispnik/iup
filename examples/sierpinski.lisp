(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-cd" "cd")))

(defpackage #:iup-examples.sierpinksi
  (:use #:common-lisp)
  (:export #:sierpinksi))

(in-package #:iup-examples.sierpinksi)

(defparameter *levels* 0)

(defun sierpinski ()
  (iup:with-iup ()
    (let* ((canvas (iup:canvas :rastersize "200x200"))
           (spin (iup:text :spin "YES" :spinmin 0 :spinmax 4))
           (vbox (iup:vbox (list canvas spin) :alignment "ACENTER"))
           (dialog (iup:dialog vbox :title "Sierpinski Carpet")))
      (setf (iup:callback canvas :map_cb) 'canvas-map
            (iup:callback canvas :unmap_cb) 'canvas-unmap
            (iup:callback canvas :action) 'canvas-redraw
            (iup:callback spin :spin_cb) 'canvas-spin
            *levels* 0)
      (iup:show-xy dialog iup:+center+ iup:+center+)
      (iup:main-loop))))

(defun sierpinski-draw (canvas level)
  (multiple-value-bind
        (w h)
      (cd:size canvas)
    (labels ((square (x y x-size y-size)
               (cd:box canvas x (+ x x-size) y (+ y y-size)))
             (recurse (x y x-size y-size level)
               (let ((x-step (/ x-size 3))
                     (y-step (/ y-size 3)))
                 (square (+ x x-step) (+ y y-step) x-step y-step)
                 (when (plusp level)
                   (dolist (x-next (list x (+ x x-step) (+ x x-step x-step)))
                     (dolist (y-next (list y (+ y y-step) (+ y y-step y-step)))
                       (recurse x-next y-next x-step y-step (1- level))))))))
      (recurse 0 0 w h level))))

(defparameter *canvas* nil)

(defun canvas-redraw (handle x y)
  (cd:activate *canvas*)
  (cd:clear *canvas*)
  (setf (cd:foreground *canvas*) cd:+red+)
  (sierpinski-draw *canvas* *levels*)
  (cd:flush *canvas*)
  iup:+default+)

(defun canvas-spin (handle pos)
  (setf *levels* (iup:attribute handle :value 'integer))
  (canvas-redraw nil nil nil)
  iup:+default+)

(defun canvas-map (handle)
  (setf *canvas* (cd:create-canvas (iup-cd:context-iup-dbuffer) handle))
  iup:+default+)

(defun canvas-unmap (handle)
  (cd:kill *canvas*)
  iup:+default+)

#-sbcl (sierpinski)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (sierpinski))
