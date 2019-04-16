(defpackage #:iup-examples.image-view
  (:use #:common-lisp))

(in-package #:iup-examples.image-view)

(defun canvas-button (canvas button pressed-p x y status)
  iup:+default+)

(defparameter *canvas* nil)
(defparameter *image* nil)

(defun canvas-redraw (canvas x y)
  (format t "~S~%" (list canvas x y))
  (let ((draw-size (split-sequence:split-sequence #\x (iup:attribute canvas :drawsize 'string))))
    (format t "~S~%" (iup:attribute canvas :drawsize 'string)))
  (finish-output)
  (cd:activate *canvas*)
  (setf (cd:background *canvas*) cd:+gray+)
  (cd:clear *canvas*)
  (cd-im:put-im-image
   canvas
   *image*
   0 0 0 0
   )
  ;; (im-image:width *image*) (im-image:height *image*)
  (cd:flush canvas)
  iup:+default+)

(defun canvas-map (canvas)
  (setf *canvas* (cd:create-canvas (iup-cd:context-iup-dbuffer) canvas))
  iup:+default+)

(defun dialog-close (dialog)
  (cd:kill *canvas*)
  (when *image*
    (im-image:destroy *image*)
    (setf *image* nil))
  iup:+close+)

(defun show-image (filename dialog)
  (when *image*
    (im-image:destroy *image*))
  (setf *image* (im-file:image-load-bitmap filename))
  (setf (iup:attribute dialog :title) filename)
  (canvas-redraw *canvas* 0 0))

(defun image-view ()
  (iup:with-iup ()
    (let* ((canvas (iup:canvas :button_cb 'canvas-button
			       :action 'canvas-redraw
			       :map_cb 'canvas-map
			       :scrollbar "YES"))
	   (dialog (iup:dialog canvas
			       :close_cb 'dialog-close
			       :size "HALFxHALF")))
      (iup:set-global "GLOBALLAYOUTDLGKEY" "YES")
      (iup:show dialog)
      (show-image "/usr/share/backgrounds/pop/nasa-89127.jpg" dialog)
      (iup:main-loop))))

;;; TODO scrollbars should pan the image

#+nil
(image-view)
