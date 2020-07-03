(defpackage #:iup-plot
  (:use #:common-lisp)
  (:export #:open
	   #:plot
	   #:begin
	   #:end
	   #:with-plot
	   #:add
	   #:add-segment
	   #:add-string
           #:add-samples
           #:add-string-samples
	   #:load-data
	   #:insert
	   #:insert-segment
	   #:insert-string
	   #:insert-samples
           #:insert-string-samples
	   #:sample
	   #:sample-string
	   #:sample-selection
	   #:sample-extra
	   #:transform
	   #:transform-to
	   #:paint-to)
  (:import-from #:tecgraf-base
                #:defalias)
  (:shadow #:open))

(in-package #:iup-plot)

(iup::defiupclasses "IUP-PLOT")

(defalias open  #'iup-plot-cffi::%iup-plot-open)
(defalias begin #'iup-plot-cffi::%iup-plot-begin)
(defalias end   #'iup-plot-cffi::%iup-plot-end)

(defmacro with-plot ((handle &key x-labels) &body body)
  (let ((handle-gensym (gensym))
	(result (gensym)))
    `(let ((,handle-gensym ,handle)
	   ,result)
       (unwind-protect
	    (progn
	      (iup-plot:begin ,handle-gensym ,x-labels)
	      ,@body)
	 (setf ,result (iup-plot:end ,handle-gensym)))
       ,result)))

(defun add (handle x y)
  (iup-plot-cffi::%iup-plot-add handle (coerce x 'double-float) (coerce y 'double-float)))

(defun add-segment (handle x y)
  (iup-plot-cffi::%iup-plot-add-segment handle (coerce x 'double-float) (coerce y 'double-float)))

(defun add-string (handle x-label y)
  (iup-plot-cffi::%iup-plot-add-str handle x-label (coerce y 'double-float)))

(defalias load-data #'iup-plot-cffi::%iup-plot-load-data)

(defun insert (handle ds-index sample-index x y)
  (iup-plot-cffi::%iup-plot-insert
   handle ds-index sample-index (coerce x 'double-float) (coerce y 'double-float)))

(defun insert-segment (handle ds-index sample-index x y)
  (iup-plot-cffi::%iup-plot-insert-segment
   handle ds-index sample-index (coerce x 'double-float) (coerce y 'double-float)))

(defun insert-string ( handle ds-index sample-index x y)
  (iup-plot-cffi::%iup-plot-insert-str
   handle ds-index sample-index (coerce x 'double-float) (coerce y 'double-float)))

(defun sequence-to-double-float-vector (sequence)
  (map '(vector double-float *)
       #'(lambda (x) (coerce x 'double-float))
       sequence))

(defun sequence-to-string-vector (sequence)
  (map '(vector string *)
       #'(lambda (x) (coerce x 'string))
       sequence))

(defun insert-string-samples (handle ds-index sample-index x-strings-sequence y-sequence)
  (let ((x-length (length x-strings-sequence))
	(y-length (length y-sequence)))
    (assert (= x-length y-length))
    (cffi:with-foreign-array
        (x-strings-ptr (sequence-to-string-vector x-strings-sequence) `(:array :string ,x-length))
      (cffi:with-foreign-array
          (y-ptr (sequence-to-double-float-vector y-sequence) `(:array :double ,x-length))
        (iup-plot-cffi::%iup-plot-insert-str-samples handle ds-index sample-index x-strings-ptr y-ptr x-length)))))

(defun insert-samples (handle ds-index sample-index x-sequence y-sequence)
  (let ((x-length (length x-sequence))
	(y-length (length y-sequence)))
    (assert (= x-length y-length))
    (cffi:with-foreign-array
        (x-ptr (sequence-to-double-float-vector x-sequence) `(:array :double ,x-length))
      (cffi:with-foreign-array
          (y-ptr (sequence-to-double-float-vector y-sequence) `(:array :double ,x-length))
	(iup-plot-cffi::%iup-plot-insert-samples handle ds-index sample-index x-ptr y-ptr x-length)))))

(defalias sample           #'iup-plot-cffi::%iup-plot-get-sample)
(defalias sample-string    #'iup-plot-cffi::%iup-plot-get-sample-str)
(defalias sample-selection #'iup-plot-cffi::%iup-plot-get-sample-selection)
;;; FIXME ^^
;;; FIXME setf (value x y) for set samples

(defun add-string-samples (handle ds-index sample-index x-strings-sequence y-sequence)
  (let ((x-length (length x-strings-sequence))
	(y-length (length y-sequence)))
    (assert (= x-length y-length))
    (cffi:with-foreign-array
        (x-strings-ptr (sequence-to-string-vector x-strings-sequence) `(:array :string ,x-length))
      (cffi:with-foreign-array
          (y-ptr (sequence-to-double-float-vector y-sequence) `(:array :double ,x-length))
        (iup-plot-cffi::%iup-plot-add-str-samples handle ds-index sample-index x-strings-ptr y-ptr x-length)))))

(defun add-samples (handle ds-index sample-index x-sequence y-sequence)
  (let ((x-length (length x-sequence))
	(y-length (length y-sequence)))
    (assert (= x-length y-length))
    (cffi:with-foreign-array
        (x-ptr (sequence-to-double-float-vector x-sequence) `(:array :double ,x-length))
      (cffi:with-foreign-array
          (y-ptr (sequence-to-double-float-vector y-sequence) `(:array :double ,x-length))
	(iup-plot-cffi::%iup-plot-add-samples handle ds-index sample-index x-ptr y-ptr x-length)))))

(defun (setf sample-extra) (new-value handle ds-index sample-index)
  (iup-plot-cffi::%iup-plot-set-sample-extra handle ds-index sample-index (coerce new-value 'double-float))
  new-value)

(defun transform (handle x y)
  (cffi:with-foreign-objects
      ((ix :double)
       (iy :double))
    (iup-plot-cffi::%iup-plot-transform handle (coerce x 'double-float) (coerce y 'double-float) ix iy)
    (values (cffi:mem-ref ix :double)
	    (cffi:mem-ref iy :double))))

(defun transform-to (handle cnv-x cnv-y)
  (cffi:with-foreign-objects
      ((ix :double)
       (iy :double))
    (iup-plot-cffi::%iup-plot-transform-to handle (coerce cnv-x 'double-float) (coerce cnv-y 'double-float) ix iy)
    (values (cffi:mem-ref ix :double)
	    (cffi:mem-ref iy :double))))

(defun find-sample (handle cnv-x cnv-y)
  (cffi:with-foreign-objects
      ((ds-index-ptr :double)
       (sample-index-ptr :double))
    (iup-plot-cffi::%iup-plot-find-sample
     handle (coerce cnv-x 'double-float) (coerce cnv-y 'double-float) ds-index-ptr sample-index-ptr)
    (values (cffi:mem-ref ds-index-ptr :double)
	    (cffi:mem-ref sample-index-ptr :double))))

(defun find-segment (handle cnv-x cnv-y)
  (cffi:with-foreign-objects
      ((ds-index-ptr :double)
       (sample1-index-ptr :double)
       (sample2-index-ptr :double))
    (iup-plot-cffi::%iup-plot-find-segment handle
                                           (coerce cnv-x 'double-float)
                                           (coerce cnv-y 'double-float)
                                           ds-index-ptr
                                           sample1-index-ptr
                                           sample2-index-ptr)
    (values (cffi:mem-ref ds-index-ptr :double)
	    (cffi:mem-ref sample1-index-ptr :double)
            (cffi:mem-ref sample2-index-ptr :double))))

(defalias paint-to #'iup-plot-cffi::%iup-plot-paint-to)
