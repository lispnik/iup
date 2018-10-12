(in-package #:iup-plottest)

(cffi:defcallback action-cb :int ((handle iup-cffi::ihandle))
  (declare (ignore handle))
  iup::+default+)

(defun plot-1 ()
  (let ((plot (iup-plot:plot :title "AutoScale"
			     :font "Helvetica"
			     :legendshow "YES"
			     :axs_xlabel "gnu (foo)"
			     :axs_ylabel "Space (m^3)"
			     :axs_xcrossorigin "YES"
			     :axs_ycrossorigin "YES"
			     :ds_linewidth 3
			     :ds_legend "Curve 1")))
    ;; add example
    (iup-plot:with-plot (plot)
      (loop with the-fac = 1e-6
	    for the-i from -100 to 100
	    for x = (+ the-i 50)
	    for y = (* the-fac (expt the-i 3))
	    do (iup-plot:add plot x y)))
    ;; insert example
    (let ((index (iup-plot:with-plot (plot)
		   (loop with the-fac = 0.02
			 for the-i from -100 below 0
			 for x = the-i
			 for y = (* (- the-fac) the-i)
			 do (iup-plot:add plot x y))))
	  (px (make-array 210 :element-type 'double-float))
	  (py (make-array 210 :element-type 'double-float)))
      (loop with the-fac = 0.02
	    for the-i from 0 to 100
	    for count from 0
	    for x = the-i
	    for y = (* (- the-fac) the-i)
	    do (setf (aref px the-i) x
		     (aref py the-i) y)
	       ;; FIXME
	    finally (iup-plot:insert-samples plot index 100 px py count)))))

(defun plottest ()
  (iup:with-iup ()
    (iup-controls:open)
    (iup-plot:open)
    (let* ((plot (plot-1))
	   (vbox (iup:vbox (list plot)))
	   (dialog (iup:dialog vbox :title "IUP Plot Test" :rastersize "800x600")))
      (iup:show-xy dialog iup:+center+ iup:+center+)
      (iup:main-loop))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (plottest))
