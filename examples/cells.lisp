(in-package #:iup-examples.cells)

(cffi:defcallback nlines-cb :int ((handle iup-cffi::ihandle)) 8)
(cffi:defcallback ncols-cb :int ((handle iup-cffi::ihandle)) 8)
(cffi:defcallback height-cb :int ((handle iup-cffi::ihandle)) 50)
(cffi:defcallback width-cb :int ((handle iup-cffi::ihandle)) 50)

(cffi:defcallback draw-cb :int
    ((handle iup-cffi::ihandle)
     (i :int)
     (j :int)
     (xmin :int)
     (xmax :int)
     (ymin :int)
     (ymax :int)
     (canvas cd-cffi::cd-canvas))
  (declare (ignore handle))
  (if (or (and (oddp i) (oddp j)) (and (oddp (1+ i)) (oddp (1+ j))))
      (cd-cffi::%cd-canvas-foreground canvas 0)
      (cd-cffi::%cd-canvas-foreground canvas #xffffff))
  (cd-cffi::%cd-canvas-box canvas xmin xmax ymin ymax)
  iup::+default+)

(defun checkerboard ()
  (iup:with-iup ()
    (iup-controls:open)
    (let* ((cells (iup-controls:cells
		   :draw_cb 'draw-cb
		   :width_cb 'width-cb
		   :height_cb 'height-cb
		   :nlines_cb 'nlines-cb
		   :ncols_cb 'ncols-cb))
	   (vbox (iup:vbox (list cells)))
	   (dialog (iup:dialog vbox :title "IupCells" :rastersize "440x480")))
      (iup:show-xy dialog iup:+center+ iup:+center+)
      (iup:main-loop))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (checkerboard))
