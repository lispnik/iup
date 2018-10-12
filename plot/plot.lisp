(in-package #:iup-plot)

(alias 'open #'iup-plot-cffi::%iup-plot-open)

(iup::defattributefun plot () (iup-plot-cffi::%iup-plot))

(alias 'begin #'iup-plot-cffi::%iup-plot-begin)
(alias 'end #'iup-plot-cffi::%iup-plot-end)

(defmacro with-plot ((handle &optional string-x-p) &body body)
  (let ((handle-gensym (gensym))
	(result (gensym)))
    `(let ((,handle-gensym ,handle)
	   ,result)
       (unwind-protect
	    (progn
	      (iup-plot:begin ,handle-gensym ,string-x-p)
	      ,@body)
	 (setf ,result (iup-plot:end ,handle-gensym)))
       ,result)))

(defun add (handle x y)
  (iup-plot-cffi::%iup-plot-add handle (coerce x 'double-float) (coerce y 'double-float)))

(alias 'add-segment	#'iup-plot-cffi::%iup-plot-add-segment)
(alias 'add-string	#'iup-plot-cffi::%iup-plot-add-str)
(alias 'load-data	#'iup-plot-cffi::%iup-plot-load-data)

(alias 'insert #'iup-plot-cffi::%iup-plot-insert)
(alias 'insert-string #'iup-plot-cffi::%iup-plot-insert-str)
(alias 'insert-segment #'iup-plot-cffi::%iup-plot-insert-segment)

;;; FIXME
;; (alias 'insert-string-samples #'iup-plot-cffi::%iup-plot-insert-str-samples)

(defun sequence-to-double-float-vector (sequence)
  (map '(vector double-float *)
       #'(lambda (x) (coerce x 'double-float))
       sequence))

(defun insert-samples (handle ds-index sample-index x-sequence y-sequence)
  (let ((x-length (length x-sequence))
	(y-length (length y-sequence)))
    (assert (= x-length y-length))
    (cffi:with-foreign-array (x-ptr (sequence-to-double-float-vector x-sequence) `(:array :double ,x-length))
      (cffi:with-foreign-array (y-ptr (sequence-to-double-float-vector y-sequence) `(:array :double ,x-length))
	(iup-plot-cffi::%iup-plot-insert-samples handle ds-index sample-index x-ptr y-ptr x-length)))))

;; (alias 'add-samples #'iup-plot-cffi::%iup-plot-add-samples)
;; (alias 'add-string-samples #'iup-plot-cffi::%iup-plot-add-str-samples)

(alias 'sample #'iup-plot-cffi::%iup-plot-get-sample)
(alias 'sample-string #'iup-plot-cffi::%iup-plot-get-sample-str)
(alias 'sample-selection #'iup-plot-cffi::%iup-plot-get-sample-selection)

(alias 'sample-extra #'iup-plot-cffi::%iup-plot-get-sample-extra)

;; (defstruct sample
;;   (x 0 :type double)
;;   (y 0 :type double))

;; (defstruct sample-string
;;   (x 0 :type string)
;;   (y :type double))

;; (defun (setf sample) (new-value handle ds-index sample-index)
;;   (multiple-value-bind (x y) new-value
;;     (print new-value)
;;     (list x y)
;;     ))

;; (defun (setf foo) (new-value) (multiple-value-bind (x y) new-value (list x y)))

;; (setf (foo) (values 1 2))

;; (setf (sample nil nil nil) (cons 1 2))

;; (defun (setf sample-string) ())
;; (defun (setf sample-selection ()))
;; (defun (setf sample-extra ()))

;;; TODO
;; (alias 'transform #'iup-plot-cffi::%iup-plot-transform
;; (alias 'transform-to #'iup-plot-cffi::%iup-plot-transform-to
;; (alias 'find-sample #'iup-plot-cffi::%iup-plot-find-sample
;; (alias 'find-segment #'iup-plot-cffi::%iup-plot-find-segment

(alias 'paint-to #'iup-plot-cffi::%iup-plot-paint-to)




