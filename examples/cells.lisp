(defpackage #:iup-examples.cells-checkerboard
  (:use #:common-lisp)
  (:export #:cells-checkerboard))

(in-package #:iup-examples.cells-checkerboard)

(defun nlines (handle) 8)
(defun ncols (handle) 8)
(defun height (handle) 50)
(defun width (handle) 50)

(defun draw (handle i j xmin xmax ymin ymax canvas)
  (declare (ignore handle))
  (if (or (and (oddp i) (oddp j)) (and (oddp (1+ i)) (oddp (1+ j))))
      (setf (cd:foreground canvas) cd:+black+)
      (setf (cd:foreground canvas) cd:+white+))
  (cd:box canvas xmin xmax ymin ymax)
  iup::+default+)

(defun checkerboard ()
  (iup:with-iup ()
    (iup-controls:open)
    (let* ((cells (iup-controls:cells
		   :draw_cb 'draw
		   :width_cb 'width
		   :height_cb 'height
		   :nlines_cb 'nlines
		   :ncols_cb 'ncols))
	   (vbox (iup:vbox (list cells)))
	   (dialog (iup:dialog vbox :title "IupCells" :rastersize "440x480")))
      (iup:show-xy dialog iup:+center+ iup:+center+)
      (iup:main-loop))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (cells-checkerboard))
