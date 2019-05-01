(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-cd" "cd-im" "im")))

(defpackage #:iup-examples.pickup52
  (:use #:common-lisp)
  (:export #:pickup52))

(in-package #:iup-examples.pickup52)

(defun load-pack ()
  (mapcar #'(lambda (file)
	      (im-file:image-load file))
	  (directory "/home/mkennedy/Projects/local-projects/lispnik/iup/examples/playingcards/*.bmp")))

(defparameter *pack* '())
(defparameter *canvas* nil)

(defun map-callback (handle)
  (setf *canvas* (cd:create-canvas (iup-cd:context-iup-dbuffer) handle))
  iup:+default+)

(defun unmap-callback (handle)
  (cd:kill *canvas*)
  iup:+default+)

(defun redraw-callback (handle posx posy)
  (cd:activate *canvas*)
  (setf (cd:background *canvas*) cd:+white+)
  (cd:clear *canvas*)
  (multiple-value-bind (width height)
      (cd:size *canvas*)
    (setf (cd:origin *canvas*) (values (/ width) (/ height)))
    (dolist (im-image *pack*)
      (let* ((sx 1.0)
	     (sy 1.0)
	     (dx (random width))
	     (dy (random height))
	     (angle (random (* 2 pi))))
	(setf (cd:transform *canvas*)
	      (list (* sx (cos angle))
		    (sin angle)
		    (- (sin angle))
		    (* sy (cos angle))
		    dx
		    dy)))
      (let ((im-width (im-image:width im-image))
	    (im-height (im-image:height im-image)))
	(cd-im:put-im-image *canvas* im-image 0 0 (/ im-width 3) (/ im-height 3)))))
  (cd:flush *canvas*)
  iup:+default+)

(defun pickup52 ()
  (setf *pack* (load-pack))
  (iup:with-iup ()
    (let* ((canvas (iup:canvas :map_cb 'map-callback
			       :unmap_cb 'unmap-callback
			       :action 'redraw-callback))
	   (vbox (iup:vbox (list canvas)))
           (dialog (iup:dialog vbox
			       :title "Pickup 52"
			       :rastersize "800x600")))
      (iup:show dialog)
      (iup:main-loop))))

#-sbcl (pickup52)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (pickup52))
