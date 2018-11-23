(defpackage #:iup-examples.canvas
  (:use #:common-lisp
	#:alexandria)
  (:export #:canvas))

(in-package #:iup-examples.canvas)

;;; A port of the example at https://notabug.org/GermanGT/eiffel-iup/src/master/examples/canvas_example.e

(defparameter *canvas* nil)
(defparameter *angle* 0)

(defun canvas-redraw (canvas angle)
  (cd:activate canvas)
  (cd:transform-rotate canvas angle)
  (setf (cd:background canvas) cd:+white+)
  (cd:clear canvas)
  (setf (cd:interior-style canvas) :interior-solid
	(cd:line-width canvas) 3
	(cd:foreground canvas) cd:+blue+)
  (cd:line canvas 40 40 450 40)
  (cd:line canvas 45 65 455 65)
  (setf (cd:line-style canvas) :line-dotted
	(cd:foreground canvas) cd:+red+)
  (cd:rect canvas 60 200 80 175)
  (setf (cd:line-style canvas) :line-continuous)
  (cd:arc canvas 150 200 100 50 0 150)
  (setf (cd:foreground canvas) cd:+yellow+)
  (cd:box canvas 240 470 80 275)
  (setf (cd:foreground canvas) cd:+magenta+)
  ;; (cd:sector canvas 260 200 100 50 0 150)
  ;; (setf (cd:text-orientation canvas) 20)
  ;; (cd:text canvas (format nil "~A ~A, IUP ~A" (lisp-implementation-type) (lisp-implementation-version) (iup:version)))
  (cd:flush canvas)
  iup:+default+)

(defun canvas-redraw-db (handle x y)
  (canvas-redraw *canvas* *angle*))

(defun canvas ()
  (sb-posix:setenv "CD_QUIET" "NO" 1)
  (iup:with-iup ()
    (let* ((angle 0)
	   (label (iup:label :title "IUP CD Example"))
	   (button (iup:button :title "Rotate 30°"
			       :action #'(lambda (self) (print "TODO change angle"))))
	   (canvas (iup:canvas :size "300x150" :action 'canvas-redraw-cb))
	   (vbox (iup:vbox (list canvas)))
	   (dialog (iup:dialog vbox)))
      ;; (cd-context-plus:initialize)
      ;; (cd:use-context-plus t)
      (setf *canvas* (cd:create-canvas (iup-cd:context-iup) canvas))
      (iup:show dialog))
    (iup:main-loop)))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (canvas))
