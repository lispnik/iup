(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-plot" "cd")))

(defpackage #:iup-examples.plottest
  (:use #:common-lisp)
  (:export #:plottest))

(in-package #:iup-examples.plottest)

(defun plot-0 ()
  (let ((plot (iup-plot:plot :title "AutoScale"
                             :font "Helvetica"
                             :legendshow :yes
                             :axs_xlabel "gnu (foo)"
                             :axs_ylabel "Space (m^3)"
                             :axs_xcrossorigin :yes
                             :axs_ycrossorigin :yes)))
    ;; add example
    (iup-plot:with-plot (plot)
      (loop :with the-fac := 1e-6
            :for the-i :from -100 :to 100
            :for x := (+ the-i 50)
            :for y := (* the-fac (expt the-i 3))
            :do (iup-plot:add plot x y)))
    (setf (iup:attribute plot :ds_legend) "Curve 1"
          (iup:attribute plot :ds_linewidth) 3)
    ;; insert example
    (let ((index (iup-plot:with-plot (plot)
                   (loop :with the-fac := 0.02
                         :for the-i :from -100 :below 0
                         :for x := the-i
                         :for y = (* (- the-fac) the-i)
                         :do (iup-plot:add plot x y))))
          (px (make-array 210))
          (py (make-array 210)))
      (loop :with the-fac := 0.02
            :for the-i :from 0 :to 100
            :for count :from 0 
            :for x := the-i
            :for y := (* (- the-fac) the-i)
            :do (setf (aref px the-i) x
                      (aref py the-i) y)
            :finally (iup-plot:insert-samples plot index 100 px py))
      (setf (iup:attribute plot :ds_legend) "Line 1")
      (iup-plot:with-plot (plot)
        (loop :for the-i :from -100 :to 100
              :for x := (- (* 0.01 the-i the-i) 30)
              :for y := (* 0.01 the-i )
              :do (iup-plot:add plot x y)))
      (setf (iup:attribute plot :ds_legend) "Curve 2"))
    plot))

(defun plot-1 ()
  (let ((plot (iup-plot:plot :title "No Autoscale+No CrossOrigin"
                             :font "Helvetica, 10"
                             :bgcolor "0 192 192"
                             :axs_xlabel "Tg (X)"
                             :axs_ylabel "Tg (Y)"
                             :axs_xautomin :no
                             :axs_yautomin :no
                             :axs_xautomax :no
                             :axs_yautomax :no
                             :axs_xmin 10
                             :axs_xmax 60
                             :axs_ymin -0.5
                             :axs_ymax 0.5
                             :axs_xfontstyle :italic
                             :axs_yfontstyle :bold
                             :axs_xreverse :yes
                             :gridcolor "128 255 128"
                             :gridlinestyle :dotted
                             :grid :yes
                             :legendshow :yes
                             :axs_xlabelcentered :yes
                             :axs_ylabelcentered :yes
                             :graphicsmode :imagergb)))
    (iup-plot:with-plot (plot)
      (loop :with the-fac := 1e-6
            :for the-i :from 0 :to 100
            :for x := the-i
            :for y := (* the-fac (expt the-i 3))
            :do (iup-plot:add plot x y)))
    (iup-plot:with-plot (plot)
      (loop :with the-fac := 0.02
            :for the-i :from 0 :to 100
            :for x := the-i
            :for y := (- (* the-fac the-i))
            :do (iup-plot:add plot x y)))
    plot))

(defun plot-2 ()
  (let ((plot (iup-plot:plot :title "Log Scale"
                             :titlefontsize 16
                             :margintop 40
                             :marginleft 70
                             :marginbottom 60
                             :grid :yes
                             :axs_xscale :log10
                             :axs_yscale :log2
                             :axs_xlabel "Tg (X)"
                             :axs_ylabel "Tg (Y)"
                             :axs_xfontstyle :bold
                             :axs_yfontstyle :bold)))
    (iup-plot:with-plot (plot)
      (loop :with the-fac := 1e-6
            :for the-i :from 0 :to 100
            :for x := (+ 0.0001 (* the-i 0.001))
            :for y := (+ 0.01 (* the-fac (expt the-i 3)))
            :do (iup-plot:add plot x y)))
    (setf (iup:attribute plot :ds_color)  "100 100 200"
          (iup:attribute plot :ds_linestyle) "DOTTED")
    plot))

(defun plot-3 ()
  (let ((plot (iup-plot:plot :title "Bar Mode")))
    (iup-plot:with-plot (plot :x-labels t)
      (loop :for label :in '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec")
            :for data :in '(10 20 30 40 50 60 70 80 90 0 10 20)
            :do (iup-plot:add-string plot label data)))
    (setf (iup:attribute plot :ds_color) "100 100 200"
          (iup:attribute plot :ds_mode) :bar)
    plot))

(defun plot-4 ()
  (let ((plot (iup-plot:plot :title "Marks Mode"
                             :axs_xautomin :no
                             :axs_xautomax :no
                             :axs_yautomin :no
                             :axs_yautomax :no
                             :axs_xmin 0
                             :axs_xmax 0.011
                             :axs_ymin 0
                             :axs_ymax 0.22
                             :axs_xtickformat "%1.3f"
                             :legendshow :yes
                             :legendpos :bottomright)))
    (iup-plot:with-plot (plot)
      (loop :with the-fac := 1e-6
            :for the-i :from 0 :to 10
            :for x := (+ 0.0001 (* the-i 0.001))
            :for y := (+ 0.01 (* the-fac the-i the-i))
            :do (iup-plot:add plot x y)))
    (setf (iup:attribute plot :ds_mode) :markline)
    (iup-plot:with-plot (plot)
      (loop :with the-fac := 1e-6
            :for the-i from 0 :to 10
            :for x := (+ 0.0001 (* the-i 0.001))
            :for y := (- 0.2 (* the-fac the-i the-i))
            :do (iup-plot:add plot x y)))
    (setf (iup:attribute plot :ds_mode) :mark
          (iup:attribute plot :ds_markstyle) :hollow_circle)
    plot))

(defun plot-5 ()
  (let ((plot (iup-plot:plot :title "Data Selection and Editing"))
        (filename
          (namestring (make-pathname :name "plot" :type "dat" :defaults #.(or *compile-file-truename* *load-truename*)))))
    (iup-plot:load-data plot filename nil)
    (setf (iup:attribute plot :ds_color) "100 100 200"
          (iup:attribute plot :editablevalues) :yes
          (iup:attribute plot :readonly) :no)
    (setf (iup:callback plot :delete_cb)
          (lambda (handle index sample-index x y)
            (iup:message "Delete Callback" 
                         (format nil "~S" (list :index index
                                                :sample-index sample-index
                                                :x x :y y)))
            iup:+default+)
          (iup:callback plot :select_cb)
          (lambda (handle index sample-index x y select)
            (iup:message "Select Callback"
                         (format nil "~S" (list :index index
                                                :sample-index sample-index
                                                :x x :y y
                                                :select select)))
            iup:+default+)
          (iup:callback plot :postdraw_cb)
          (lambda (handle canvas)
            (multiple-value-bind
                  (ix iy)
                (iup-plot:transform handle 0.003 0.02)
              (setf (cd:font-style canvas) :font-style-bold
                    (cd:font-size canvas) 10
                    (cd:text-alignment canvas) :alignment-south)
              (cd:text canvas ix iy "My Inline Legend"))
            iup:+default+)
          (iup:callback plot :predraw_cb)
          (lambda (handle canvas)
            iup:+default+)) 
    plot))

(defun plot-6 ()
  (let ((plot (iup-plot:plot :title "Horizontal Bar Mode")))
    (iup-plot:with-plot (plot)
      (loop :for label :in '(10 20 30 40 50 60 70 80 90 0 10 20)
            :for data :in (loop :for i :from 1 :to 12 :collect i)
            :do (iup-plot:add plot label data)))
    (setf (iup:attribute plot :ds_color) "100 100 200"
          (iup:attribute plot :ds_mode) :horizontalbar)
    plot))

(defun plot-7 ()
  (let ((plot (iup-plot:plot :title "Step Curve")))
    (iup-plot:with-plot (plot)
      (loop :with pi2 := (* 2 pi)
            :for x :from 0 :to pi2 :by (/ pi2 40)
            :for y := (sin x)
            :do (iup-plot:add plot x y)))
    (setf (iup:attribute plot :ds_mode) :step)
    plot))

(defun plot-8 ()
  (let ((plot (iup-plot:plot :title "Stem Mode" :legendshow :yes)))
    (iup-plot:with-plot (plot)
      (loop :for x :from 0 :to pi :by (/ pi 10)
            :for y := (cos x)
            :do (iup-plot:add plot x y)))
    (setf (iup:attribute plot :ds_mode) :markstem
          (iup:attribute plot :ds_legend) "cos")
    plot))

(defun plot-9 ()
  (let ((plot (iup-plot:plot :title "Multi Bar Mode"))
        (data '(10 20 30 40 50 60 70 80 90 0 10 20))
        (months '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec")))
    (loop :repeat 3
          :do (progn
                (iup-plot:with-plot (plot :x-labels t)
                  (loop :for label :in months
                        :for y :in data
                        :do (iup-plot:add-string
                             plot
                             label
                             (* y (random 1.0)))))
                (setf (iup:attribute plot :ds_mode) :multibar)))
    plot))

(defun plot-10 ()
  (let ((plot (iup-plot:plot :title "Error Bar")))
    (iup-plot:with-plot (plot)
      (loop :for the-i :from 0 :to 360 :by 10
            :for x := the-i
            :for y := (sin (/ (* x pi) 180))
            :for count :from 0
            :do (iup-plot:add plot x y)
                (setf (iup-plot:sample-extra plot 0 count)
                      (* 0 (random 0.1)))))
    (setf (iup:attribute plot :ds_mode) :errorbar
          (iup:attribute plot :ds_legend)  "Error Bar"
          (iup:attribute plot :highlightmode) :both
          (iup:attribute plot :datasetclipping) :areaoffset)
    plot))

(defun plot-11 ()
  (let ((plot (iup-plot:plot :title "Pie Mode")))
    (iup-plot:with-plot (plot :x-labels t)
      (loop :for label :in '("jan" "feb" "mar" "apr" "may" "jun" "jul" "aug" "sep" "oct" "nov" "dec")
            :for data :in '(10 20 30 40 50 60 70 80 90 0 10 20)
            :do (iup-plot:add-string plot label data)))
    (setf (iup:attribute plot :ds_pieslicelabel) "X"
          (iup:attribute plot :ds_mode) :pie)
    plot))

(defun plottest ()
  (iup:with-iup ()
    (iup-controls:open)
    (iup-plot:open)
    (let* ((vbox (iup:grid-box (list (plot-0)
                                     (plot-1)
                                     (plot-2)
                                     (plot-3)
                                     (plot-4)
                                     (plot-5)
                                     (plot-6)
                                     (plot-7)
                                     (plot-8)
                                     (plot-9)
                                     (plot-10)
                                     (plot-11))
                               :numdiv 6))
           (dialog (iup:dialog vbox
                               :title "IUP Plot Test"
                               :rastersize "800x600")))
      (iup:show-xy dialog iup:+center+ iup:+center+)
      (iup:main-loop))))

#-sbcl (plottest)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (plottest))
