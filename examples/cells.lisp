(defpackage #:iup-examples.cells-checkerboard
  (:use #:common-lisp)
  (:export #:cells-checkerboard))

(in-package #:iup-examples.cells-checkerboard)

(defun nlines (handle) 8)
(defun ncols (handle) 8)
(defun height (handle i) 50)
(defun width (handle j) 50)

(defun draw (handle i j xmin xmax ymin ymax canvas)
  (declare (ignore handle))
  (if (or (and (oddp i) (oddp j)) (and (oddp (1+ i)) (oddp (1+ j))))
      (setf (cd:foreground canvas) cd:+black+)
      (setf (cd:foreground canvas) cd:+white+))
  (cd:box canvas xmin xmax ymin ymax)
  iup::+default+)

(defun click (handle button pressed line column x y status)
  (declare (ignore handle))
  (iup:message "Mouse Button Clicked"
	       (format nil  "button ~S, pressed ~S, line ~S, column ~S, x ~S, y ~S, status ~S"
		       ;; FIXME provide decoder for button
		       button
		       pressed
		       line
		       column
		       x
		       y
		       ;; FIXME provide decoder for status
		       status))
  iup:+default+)

;;; FIXME for the above, see https://www.tecgraf.puc-rio.br/iup/en/call/iup_button_cb.html

(defun cells-checkerboard ()
  (iup:with-iup ()
    (iup-controls:open)
    (let* ((cells (iup-controls:cells
		   :draw_cb 'draw
		   :width_cb 'width
		   :height_cb 'height
		   :nlines_cb 'nlines
		   :ncols_cb 'ncols
		   :mouseclick_cb 'click))
	   (vbox (iup:vbox (list cells)))
	   (dialog (iup:dialog vbox :title "Cells Checkerboard" :rastersize "440x480" :shrink "YES")))
      (iup:show-xy dialog iup:+center+ iup:+center+)
      (iup:main-loop))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (cells-checkerboard))
