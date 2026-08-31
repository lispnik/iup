(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup/cd" "cd" "im")))

(defpackage #:iup-examples.image-view
  (:use #:common-lisp)
  (:export #:image-view))

(in-package #:iup-examples.image-view)

(defun canvas-button (canvas button pressed-p x y status)
  (declare (ignore canvas button pressed-p x y status))
  iup:+default+)

(defparameter *canvas* nil)
(defparameter *image* nil)

(defun canvas-redraw (handle x y)
  (declare (ignore handle x y))
  (cd:activate *canvas*)
  (setf (cd:background *canvas*) cd:+gray+)
  (cd:clear *canvas*)
  (when *image*
    (cd:put-image *canvas* *image*))
  (cd:flush *canvas*)
  iup:+default+)

(defun canvas-map (canvas)
  (setf *canvas* (cd:make-canvas (iup-cd:context-iup-dbuffer) canvas))
  iup:+default+)

(defun dialog-close (dialog)
  (declare (ignore dialog))
  (cd:kill *canvas*)
  (when *image*
    (im:destroy *image*)
    (setf *image* nil))
  iup:+close+)

(defun show-image (filename dialog)
  (when *image*
    (im:destroy *image*))
  (setf *image* (im:load filename))
  (setf (iup:attribute dialog :title) (namestring filename))
  (canvas-redraw nil 0 0))

(defun image-view (&optional (filename
                              (asdf:system-relative-pathname
                               "iup" "examples/lispalien.ico")))
  (iup:with-iup ()
    (let* ((canvas (iup:canvas :button_cb 'canvas-button
			       :action 'canvas-redraw
			       :map_cb 'canvas-map
			       :scrollbar "YES"))
	   (dialog (iup:dialog canvas
			       :close_cb 'dialog-close
			       :size "HALFxHALF")))
      (iup:show dialog)
      (show-image filename dialog)
      (iup:main-loop))))

;;; TODO scrollbars should pan the image

#+nil
(image-view)
