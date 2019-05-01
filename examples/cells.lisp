(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-controls" "cd")))

(defpackage #:iup-examples.cells-checkerboard
  (:use #:common-lisp)
  (:export #:cells-checkerboard))

(in-package #:iup-examples.cells-checkerboard)

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

(defun nlines (handle) 8)
(defun ncols (handle) 8)
(defun height (handle i) 50)
(defun width (handle j) 50)

(defun draw (handle i j xmin xmax ymin ymax canvas)
  (if (or (and (oddp i) (oddp j)) (and (oddp (1+ i)) (oddp (1+ j))))
      (setf (cd:foreground canvas) cd:+black+)
      (setf (cd:foreground canvas) cd:+white+))
  (cd:box canvas xmin xmax ymin ymax)
  iup::+default+)

(defun click (handle button pressed line column x y status)
  (iup:message
   "Clicked!"
   (format nil "Callback arguments~%~S"
    (list :button button
          :pressed pressed
          :line line
          :column column
          :x x
          :y y
          :status (iup:status-plist status))))
     iup:+default+)

#-sbcl (cells-checkerboard)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (cells-checkerboard))
