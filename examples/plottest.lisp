(in-package #:iup-plottest)

(cffi:defcallback action-cb :int ((handle iup-cffi::ihandle))
  (declare (ignore handle))
  iup::+default+)

(defun plot-0 ()
  (let ((plot (iup-plot:plot :title "AutoScale"
			     :font "Helvetica"
			     :legendshow "YES"
			     :axs_xlabel "gnu (foo)"
			     :axs_ylabel "Space (m^3)"
			     :axs_xcrossorigin "YES"
			     :axs_ycrossorigin "YES")))
    ;; add example
    (iup-plot:with-plot (plot)
      (loop with the-fac = 1e-6
	    for the-i from -100 to 100
	    for x = (+ the-i 50)
	    for y = (* the-fac (expt the-i 3))
	    do (iup-plot:add plot x y)))
    (setf (iup:attribute plot :ds_legend) "Curve 1"
	  (iup:attribute plot :ds_linewidth) 3)
    ;; insert example
    (let ((index (iup-plot:with-plot (plot)
		   (loop with the-fac = 0.02
			 for the-i from -100 below 0
			 for x = the-i
			 for y = (* (- the-fac) the-i)
			 do (iup-plot:add plot x y))))
	  (px (make-array 210))
	  (py (make-array 210)))
      (loop with the-fac = 0.02
	    for the-i from 0 to 100
	    for count from 0
	    for x = the-i
	    for y = (* (- the-fac) the-i)
	    do (setf (aref px the-i) x
		     (aref py the-i) y)
	    finally (iup-plot:insert-samples plot index 100 px py))
      (setf (iup:attribute plot :ds_legend) "Line 1")
      (iup-plot:with-plot (plot)
	(loop for the-i from -100 to 100
	      for x = (- (* 0.01 the-i the-i) 30)
	      for y = (* 0.01 the-i )
	      do (iup-plot:add plot x y)))
      (setf (iup:attribute plot :ds_legend) "Curve 2"))
    plot))

(defun plot-1 ()
  (let ((plot (iup-plot:plot :title "No Autoscale+No CrossOrigin"
			     :font "Helvetica, 10"
			     :bgcolor "0 192 192"
			     :axs_xlabel "Tg (X)"
			     :axs_ylabel "Tg (Y)"
			     :axs_xautomin "NO"
			     :axs_yautomin "NO"
			     :axs_xautomax "NO"
			     :axs_yautomax "NO"
			     :axs_xmin 10
			     :axs_xmax 60
			     :axs_ymin -0.5
			     :axs_ymax 0.5
			     :axs_xfontstyle "ITALIC"
			     :axs_yfontstyle "BOLD"
			     :axs_xreverse "YES"
			     :gridcolor "128 255 128"
			     :gridlinestyle "DOTTED"
			     :grid "YES"
			     :legendshow "YES"
			     :axs_xlabelcentered "YES"
			     :axs_ylabelcentered "YES"
			     :graphicsmode "IMAGERGB")))
    (iup-plot:with-plot (plot)
      (loop with the-fac = 1e-6
	    for the-i from 0 to 100
	    for x = the-i
	    for y = (* the-fac (expt the-i 3))
	    do (iup-plot:add plot x y)))
    (iup-plot:with-plot (plot)
      (loop with the-fac = 0.02
	    for the-i from 0 to 100
	    for x = the-i
	    for y = (- (* the-fac the-i))
	    do (iup-plot:add plot x y)))
    plot))

(defun plot-2 ()
  (let ((plot (iup-plot:plot :title "Log Scale"
			     :titlefontsize 16
			     :margintop 40
			     :marginleft 70
			     :marginbottom 60
			     :grid "YES"
			     :axs_xscale "LOG10"
			     :axs_yscale "LOG2"
			     :axs_xlabel "Tg (X)"
			     :axs_ylabel "Tg (Y)"
			     :axs_xfontstyle "BOLD"
			     :axs_yfontstyle "BOLD")))
    (iup-plot:with-plot (plot)
      (loop with the-fac = 1e-6
	    for the-i from 0 to 100
	    for x = (+ 0.0001 (* the-i 0.001))
	    for y = (+ 0.01 (* the-fac (expt the-i 3)))
	    do (iup-plot:add plot x y)))
    (setf (iup:attribute plot :ds_color)  "100 100 200"
	  (iup:attribute plot :ds_linestyle) "DOTTED")
    plot))


(defun plot-3 ()
  (let ((plot (iup-plot:plot :title "Bar Mode")))
    (iup-plot:with-plot (plot :x-labels t)
      (loop for label in '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec")
	    for data in '(10 20 30 40 50 60 70 80 90 0 10 20)
	    do (iup-plot:add-string plot label data)))
    (setf (iup:attribute plot :ds_color)  "100 100 200"
	  (iup:attribute plot :ds_mode) "BAR")
    plot))

(defun plottest ()
  (iup:with-iup ()
    (iup-controls:open)
    (iup-plot:open)
    (let* ((vbox (iup:vbox (list (plot-0)
				 (plot-1)
				 (plot-2)
				 (plot-3))))
	   (dialog (iup:dialog vbox :title "IUP Plot Test" :rastersize "800x600")))
      (iup:show-xy dialog iup:+center+ iup:+center+)
      (iup:main-loop))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (plottest))
