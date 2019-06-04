(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-controls" "im" "iup-im" "iup-imglib" "iup-plot" "closer-mop" "cl-prime-maker" "alexandria" "serapeum")))

(defpackage #:iup-examples.inspector
  (:use #:common-lisp
	#:alexandria
	#:serapeum)
  (:export #:inspect
	   #:inspect
	   #:*registered-detectors*
	   #:define-detector)
  (:import-from #:cl-prime-maker #:primep)
  (:shadow #:inspect))

(in-package #:iup-examples.inspector)

;;; https://core.ac.uk/download/pdf/4404837.pdf

(defvar *registered-detectors-plist* nil
  "Plist of currently registered detectors.")

(defvar *status-bar-length* 80
  "Length (in ems) for the status bar pretty-printed representation
  for the object under inspection.")

(defclass detector ()
  ((name :initarg :name
	 :reader name)
   (title :initarg :title
	  :reader title)
   (test-function :initarg :test-function
		  :reader test-function)
   (view-function :initarg :view-function
		  :reader view-function)))

(defmacro define-detector ((name title) &body body &key test-function view-function)
  (declare (ignore body))
  (let ((detector (gensym))
	(detector-name (gensym)))
    `(let* ((,detector-name ',name)
	    (,detector
	      (make-instance 'detector
			     :title ,title
			     :name ,detector-name
			     :test-function ,test-function
			     :view-function ,view-function)))
       (if (getf *registered-detectors-plist* ,detector-name)
	   (setf (getf *registered-detectors-plist* ,detector-name) ,detector)
	   (appendf *registered-detectors-plist* (list ,detector-name ,detector)))
       ,detector-name)))

(defun create-matrix (&rest rest &key headers (column-default-alignment :aleft) &allow-other-keys)
  (let ((matrix-ex-args (copy-list rest)))
    (remf matrix-ex-args :headers)
    (remf matrix-ex-args :column-default-alignment)
    (let* ((handle (apply #'iup-controls:matrix-ex matrix-ex-args)))
      (setf (iup:attribute-id handle :alignmentlin 0) :aleft
            (iup:attribute handle :resizematrix) :yes
            (iup:attribute handle :numcol_visible_last) :yes)
      (loop :for header :in headers
            :for c :from 1
            :do (setf
		 (iup:attribute-id handle :alignment c) column-default-alignment
		 (iup:attribute-id-2 handle nil 0 c) header))
      handle)))

(defun write-briefly (object &rest rest &key &allow-other-keys)
  (apply #'write-to-string
	 object
	 (append rest '(:level 2 :lines 1))))

(defvar *unbound-label* "")

(define-detector (:symbol-detector "&Symbol")
  :test-function #'symbolp
  :view-function
  (lambda (symbol)
    (let ((handle (create-matrix :numcol 2 :numlin 7 :headers '("Attribute" "Value"))))
      (setf (iup:attribute-id-2 handle nil 1 1) "Name"
	    (iup:attribute-id-2 handle nil 2 1) "Variable"
	    (iup:attribute-id-2 handle nil 3 1) "Variable Documentation"
	    (iup:attribute-id-2 handle nil 4 1) "Function"
	    (iup:attribute-id-2 handle nil 5 1) "Function Documentation"
	    (iup:attribute-id-2 handle nil 6 1) "Property List"
	    (iup:attribute-id-2 handle nil 7 1) "Class"
	    (iup:attribute-id-2 handle nil 8 1) "Type"
	    (iup:attribute-id-2 handle nil 9 1) "Type Specifier"
	    (iup:attribute-id-2 handle nil 1 2) (write-briefly (symbol-name symbol))
	    (iup:attribute-id-2 handle nil 2 2) (if (boundp symbol)
						    (write-briefly (symbol-value symbol))
						    *unbound-label*)
	    ;; (iup:attribute-id-2 handle nil 3 2) 
	    ;; (iup:attribute-id-2 handle nil 4 2) (write-briefly (if (fboundp symbol)
	    ;; 							   (symbol-function symbol)
	    ;; 							   *unbound-label*))
	    
	    ;; (iup:attribute-id-2 handle nil 5 2) (write-briefly (ignore-errors (find-class symbol)))
	    ;; (iup:attribute-id-2 handle nil 6 2) "TODO"
	    ;; (iup:attribute-id-2 handle nil 7 2) "TODO"
	    ;; (iup:attribute-id-2 handle nil 8 2) "TODO"
	    )
      handle)))

(define-detector (:cons-detector "&Cons")
  :test-function #'consp
  :view-function
  (lambda (object)
    (let ((handle (create-matrix :numcol 2 :numlin 2 :headers '("Place" "Value"))))
      (setf (iup:attribute-id-2 handle nil 1 1) "CAR"
	    (iup:attribute-id-2 handle nil 2 1) "CDR"
	    (iup:attribute-id handle :alignment 1) :aleft
	    (iup:attribute-id handle :alignment 2) :aleft)
      (setf (iup:attribute-id-2 handle nil 1 2) (write-briefly (car object))
	    (iup:attribute-id-2 handle nil 2 2) (write-briefly (cdr object)))
      handle)))

(define-detector (:list-detector "&List")
  :test-function #'proper-list-p
  :view-function
  (lambda (list)
    (let* ((length
	     (length list))
	   (handle
	     (create-matrix :numcol 2
			    :numlin length
			    :headers '("Index" "Value")
			    :menucontext_cb (lambda (handle menu-handle lin col)
                                              (iup:insert menu-handle
                                                          (iup:get-child menu-handle 0)
                                                          (iup:item :title "Inspect"
                                                                    :action (lambda (handle)
                                                                              (inspect (elt list (1- lin)))
                                                                              iup:+default+)))
					      iup:+default+))))
      (setf (iup:attribute-id handle :alignment 1) :aleft
	    (iup:attribute-id handle :alignment 2) :aleft)
      (loop :for i :from 0 :below length
	    :for l :from 1
	    :for e in list
	    :do (setf (iup:attribute-id-2 handle nil l 1) i
		      (iup:attribute-id-2 handle nil l 2) (write-briefly e)))
      handle)))

(define-detector (:plist-detector "&Plist")
  :test-function 
  (lambda (object) 
    (and (listp object)
	 (proper-list-p object)
	 (evenp (length object))))
  :view-function
  (lambda (plist)
    (let* ((length (length plist))
	   (handle (create-matrix :numcol 2
				  :numlin (/ length 2)
				  :headers '("Attribute" "Value"))))
      (setf (iup:attribute-id handle :alignment 1) :aleft
	    (iup:attribute-id handle :alignment 2) :aleft)
      (loop :for (key value) :on plist :by #'cddr
	    :for l :from 1
	    :do (setf (iup:attribute-id-2 handle nil l 1) (write-briefly key)
		      (iup:attribute-id-2 handle nil l 2) (write-briefly value)))
      handle)))

(define-detector (:vector-detector "&Vector")
  :test-function #'vectorp
  :view-function
  (lambda (vector)
    (let* ((length (length vector))
	   (handle (create-matrix :numcol 2 :numlin length :headers '("Index" "Value"))))
      (loop :for i :from 0 :below length
	    :for l :from 1
	    :for e :across vector
	    :do (setf (iup:attribute-id-2 handle nil l 1) i
		      (iup:attribute-id-2 handle nil l 2) (write-briefly e)))
      handle)))

(define-detector (:list-plotting-detector "&Plot")
  :test-function
  (lambda (object)
    (and (proper-list-p object) (every #'realp object)))
  :view-function
  (lambda (list)
    (let ((handle (iup-plot:plot)))
      (iup-plot:with-plot (handle)
	(loop :for i :from 0
	      :for e :in list
	      :do (iup-plot:add handle i e)))
      handle)))

(define-detector (:2d-array-detector "&Array") 
  :test-function
  (lambda (object)
    (and (arrayp object)
	 (= 2 (array-rank object))
	 (loop for i below (array-total-size object)
	       always (realp (row-major-aref object i)))))
  :view-function
  (lambda (object)
    (let* ((numcol (elt (array-dimensions object) 1))
	   (numlin (elt (array-dimensions object) 0))
	   (handle (create-matrix :numcol numcol :numlin numlin)))
      (loop :for i :below numlin
	    :for l :from 1
	    :do (setf (iup:attribute-id-2 handle nil l 0) i))
      (loop :for j :below numcol
	    :for c :from 1
	    :do (setf (iup:attribute-id-2 handle nil 0 c) j
		      (iup:attribute-id handle :alignment c) :aright))
      (loop :for i :below (elt (array-dimensions object) 0)
	    :for l :from 1
	    :do (loop :for j :below (elt (array-dimensions object) 1)
		      :for c :from 1
		      :do (setf (iup:attribute-id-2 handle nil l c)
				(write-briefly (aref object i j)))))
      handle)))

(define-detector (:2d-array-plotting-detector "&Plot")
  :test-function
  (lambda (object)
    (and (arrayp object)
	 (= 2 (array-rank object))
	 (loop :for i :below (array-total-size object)
	       :always (realp (row-major-aref object i)))))
  :view-function
  (lambda (object)
    (let ((handle
	    (iup-controls:cells
	     :ncols_cb (lambda (handle)
			 (declare (ignore handle))
			 (elt (array-dimensions object) 1))
	     :nlines_cb (lambda (handle)
			  (declare (ignore handle))
			  (elt (array-dimensions object) 0))
	     :draw_cb (lambda (handle i j xmin xmax ymin ymax canvas)
			(declare (ignore handle i j))
			;; FIXME put a nice gradient scale here
			(ecase (random 2)
			  (1 (setf (cd:foreground canvas) cd:+white+))
			  (0 (setf (cd:foreground canvas) cd:+blue+)))
			(cd:box canvas xmin xmax ymin ymax)
			iup:+default+)
	     :mouseclick_cb (lambda (handle button pressed line column x y status)
			      (declare (ignore handle button pressed x y status))
			      (iup:message
			       (format nil "Value at ~A, ~A" (1- line) (1- column))
			       (write-to-string (aref object (1- line) (1- column))))
			      iup:+default+))))
      handle)))

(define-detector (:package-detector "&Package")
  :test-function #'packagep
  :view-function
  (lambda (object)
    (let ((handle
	    (create-matrix
	     :numcol 2
	     :numlin 4
	     :headers '("Attribute" "Value")
	     :map_cb #'(lambda (handle)
			 (setf (iup:attribute-id-2 handle nil 1 2)
			       (write-briefly (package-name object))
			       (iup:attribute-id-2 handle nil 2 2)
			       (write-briefly (package-nicknames object))
			       (iup:attribute-id-2 handle nil 3 2)
			       (write-briefly (package-use-list object))
			       (iup:attribute-id-2 handle nil 4 2)
			       (write-briefly (package-used-by-list object)))
			 iup:+default+))))
      (setf (iup:attribute-id-2 handle nil 1 1) "Name"
	    (iup:attribute-id-2 handle nil 2 1) "Nicknames"
	    (iup:attribute-id-2 handle nil 3 1) "Uses"
	    (iup:attribute-id-2 handle nil 4 1) "Used by")
      handle)))

(define-detector (:float-detector "&Float")
  :test-function #'floatp
  :view-function
  (lambda (float)  
    (flet ((format-engineering (stream x)
	     (let* ((base (log (abs x) 10))
		    (scale (1+ (round (mod base 3))))
		    (format-string (format nil "~~,,,~DE" scale)))
	       (format stream format-string x))))
      (let ((handle (create-matrix :numcol 2 :numlin 8 :headers '("Attribute" "Value"))))
	(multiple-value-bind
	      (significand exponent sign)
	    (decode-float float)
	  (loop :for label :in '("Type"
				 "Scientific"
				 "Engineering"
				 "Sign"
				 "Significand"
				 "Exponent"
				 "Precision"
				 "Digits")
		:for r :from 1
		:do (setf (iup:attribute-id-2 handle nil r 1) label))
	  (setf (iup:attribute-id-2 handle nil 1 2) (write-briefly (type-of float))
		(iup:attribute-id-2 handle nil 2 2) (format nil "~E" float)
		(iup:attribute-id-2 handle nil 3 2) (format-engineering nil float)
		(iup:attribute-id-2 handle nil 4 2) sign
		(iup:attribute-id-2 handle nil 5 2) significand
		(iup:attribute-id-2 handle nil 6 2) exponent
		(iup:attribute-id-2 handle nil 7 2) (float-precision float)
		(iup:attribute-id-2 handle nil 8 2) (float-digits float)))
	handle))))

(define-detector (:package-symbols-detector "&Symbols")
  :test-function #'packagep
  :view-function
  (lambda (package)
    (let ((handle
	    (create-matrix
	     :numcol 3
	     :headers '("Symbol" "Accessibility" "Package")
	     :map_cb (lambda (handle)
		       (with-package-iterator
			   (iterator package :external :internal :inherited)
			 (loop for r from 1
			       do (multiple-value-bind
					(more-p symbol accessibility package)
				      (iterator)
				    (if more-p
					(progn
					  (setf (iup:attribute handle :addlin) r
						(iup:attribute-id-2 handle nil r 1)
						(write-to-string (symbol-name symbol))
						(iup:attribute-id-2 handle nil r 3)
						(write-to-string (package-name package))
						(iup:attribute-id-2 handle nil r 2)
						(write-to-string accessibility)))
					(return)))))
		       iup:+default+))))
      (setf (iup:attribute-id handle :alignment 1) :aleft
	    (iup:attribute-id handle :alignment 2) :aleft
	    (iup:attribute-id handle :alignment 3) :aleft)
      handle)))

(define-detector (:string-detector "&String")
  :test-function #'stringp
  :view-function
  (lambda (string)
    (let ((handle (create-matrix :numcol 2 :numlin 3 :headers '("Attribute" "Value"))))
      (setf (iup:attribute-id-2 handle nil 1 1) "Type"
	    (iup:attribute-id-2 handle nil 2 1) "Length"
	    (iup:attribute-id-2 handle nil 3 1) "Escaped (Lisp)"	    
	    (iup:attribute-id-2 handle nil 4 1) "Uppercase"
	    (iup:attribute-id-2 handle nil 5 1) "Lowercase"
	    (iup:attribute-id-2 handle nil 6 1) "Capitalize"
	    (iup:attribute-id-2 handle nil 1 2) (write-briefly (type-of string))
	    (iup:attribute-id-2 handle nil 2 2) (length string)
	    (iup:attribute-id-2 handle nil 3 2) (write-to-string string :escape t :lines 1)
	    (iup:attribute-id-2 handle nil 4 2) (write-briefly (string-upcase string))
	    (iup:attribute-id-2 handle nil 5 2) (write-briefly (string-downcase string))
	    (iup:attribute-id-2 handle nil 6 2) (write-briefly (string-capitalize string)))
      handle)))

;; (defun make-class-detector ()
;;   (make-instance 'detector
;;                  :title "&Class"
;;                  :test-function #'c2cl:classp
;;                  :view (lambda (object)
;;                          (iup:label :title "TBD"))))

;; (defun make-class-precedence-list-detector ()
;;   (make-instance 'detector
;;                  :title "&Precendence"
;;                  :test-function #'c2cl:classp
;;                  :view (lambda (object)
;;                          (let* ((precedence-list (c2mop:class-precedence-list (find-class 'c2mop:slot-definition)))
;;                                 (handle (create-matrix :numcol 1 :numlin (length precedence-list))))
;;                            (setf (iup:attribute-id-2 handle nil 0 1) "Class"
;;                                  (iup:attribute handle :fittotext) "C1")
;;                            (loop for class in precedence-list
;;                                  for r from 1
;;                                  do (setf (iup:attribute-id-2 handle nil r 1) (write-briefly class)
;;                                           (iup:attribute-id handle :alignment r) :aleft))
;;                            handle))))

;; (defun make-class-slots-detector ()
;;   (make-instance 'detector
;;                  :title "&Slots"
;;                  :test-function #'c2cl:classp
;;                  :view (lambda (object)
;;                          (let* ((class-slots (c2mop:class-direct-slots object))
;;                                 (handle (create-matrix :numcol 7 :numlin (length class-slots)))
;;                                 (slot-metadata '(("Name" c2mop:slot-definition-name)
;;                                                  ("Type" c2mop:slot-definition-type)
;;                                                  ("Readers" c2mop:slot-definition-readers)
;;                                                  ("Writers" c2mop:slot-definition-writers)
;;                                                  ("Initargs" c2mop:slot-definition-initargs)
;;                                                  ("Initform" c2mop:slot-definition-initform)
;;                                                  ("Allocation" c2mop:slot-definition-allocation))))
;;                            (loop for ((heading function)) on slot-metadata
;;                                  for c from 1
;;                                  do (setf (iup:attribute-id-2 handle nil 0 c) heading))
;;                            (loop for slot-definition in class-slots
;;                                  for r from 1
;;                                  do (loop for ((heading function)) on slot-metadata
;;                                           for c from 1
;;                                           do (setf (iup:attribute-id handle :alignment c) :aleft
;;                                                    (iup:attribute-id-2 handle nil r c) (write-briefly (funcall function slot-definition)))))
;;                            handle))))

(define-detector (:integer-detector "&Integer")
  :test-function #'integerp
  :view-function
  (lambda (object)
    (let* ((handle
	     (create-matrix :numcol 2
			    :numlin 5
			    :headers '("Attribute" "Value")
			    :map_cb (lambda (handle)
					(when (and (plusp object) (serapeum:fixnump object))
					  (setf (iup:attribute handle :addlin) 6
						(iup:attribute-id-2 handle nil 6 1)
						"Prime?"
						(iup:attribute-id-2 handle nil 6 2)
						(write-briefly (primep object))))
					iup:+default+))))
      (setf (iup:attribute-id-2 handle nil 1 1) "Binary"
	    (iup:attribute-id-2 handle nil 2 1) "Octal"
	    (iup:attribute-id-2 handle nil 3 1) "Decimal"
	    (iup:attribute-id-2 handle nil 4 1) "Hexidecimal"
	    (iup:attribute-id-2 handle nil 5 1) "Length"
	    (iup:attribute-id-2 handle nil 1 2) (write-briefly object :radix t :base 2)
	    (iup:attribute-id-2 handle nil 2 2) (write-briefly object :radix t :base 8)
	    (iup:attribute-id-2 handle nil 3 2) (write-briefly object :base 10)
	    (iup:attribute-id-2 handle nil 4 2) (write-briefly object :radix t :base 16)
	    (iup:attribute-id-2 handle nil 5 2) (write-briefly (integer-length object)))
      handle)))

(define-detector (:complex-detector "&Complex")
  :test-function #'complexp
  :view-function
  (lambda (object)
    (let* ((handle (create-matrix :numcol 2 :numlin 5 :headers '("Component" "Value"))))
      (setf (iup:attribute-id-2 handle nil 1 1) "Real"
	    (iup:attribute-id-2 handle nil 2 1) "Imaginary"
	    (iup:attribute-id-2 handle nil 3 1) "Magnitude)"
	    (iup:attribute-id-2 handle nil 4 1) "Phase (radian)"
	    (iup:attribute-id-2 handle nil 5 1) "Phase (degree)"
	    (iup:attribute-id-2 handle nil 1 2) (realpart object)
	    (iup:attribute-id-2 handle nil 2 2) (imagpart object)
	    (iup:attribute-id-2 handle nil 3 2) (abs object)
	    (iup:attribute-id-2 handle nil 4 2) (phase object)
	    (iup:attribute-id-2 handle nil 5 2) (* (/ 180 pi) (phase object)))
      handle)))

(defvar *inspector-count* 0)

(defun inspect (object)
  (let* ((applicable-detectors
	   (remove-if-not #'(lambda (detector)
			      (funcall (test-function detector) object))
			  (plist-values *registered-detectors-plist*)))
	 (views
	   (mapcar #'(lambda (detector)
		       (funcall (view-function detector) object))
		   applicable-detectors))
	 (vbox
	   (iup:vbox (list (loop :with tabs := (iup:tabs views)
				 :for detector :in applicable-detectors
				 :for i :from 0
				 :do (setf (iup:attribute-id tabs :tabtitle i)
					   (title detector))
				 :finally (return tabs))
			   ;; FIXME clean up pretty printing:
			   (iup:label :title (let ((*print-right-margin* *status-bar-length*)
						   (*print-lines* 1))
					       (with-output-to-string (stream)
						 (write-string "Inspecting " stream)
						 (pprint object stream)))
				      :expand :horizontal))))
	 (dialog (iup:dialog vbox :title (format nil "Inspector ~A" *inspector-count*))))
    (incf *inspector-count*)
    (iup:show dialog)))

(defun inspector-test ()
  (iup:with-iup ()
    (iup-controls:open)
    (iup-plot:open)
    (inspect (loop for i from 0 below (* 2 pi) by 0.1
                   collect (cos i)))
    (iup:main-loop)))

(defvar *test-object*
  (list 
   (find-class 'c2mop:slot-definition)
   (find-package "IUP")
   '(:foo "bar" :baz 43)
   '(("foo" . "bar")
     ("baz" 42)
     ("quux")
     (:key . :value))
   (let ((array #2A ((1 2 3 3)
                     (1 2 3 3)
                     (1 2 3 3)
                     (1 2 3 3))))
     (loop for i below (array-total-size array)
           do (setf (row-major-aref array i) (* (row-major-aref array i) (random 1.0)))
           finally (return array)))
   (loop for i from 0 below (* 2 pi) by 0.1
         collect (cos i))
   (iup-im:load-image (asdf:system-relative-pathname "iup" "examples/lispalien.ico"))
   *features*
   '(1.0 2 #C (4 5.0))
   (make-array 4 :initial-contents (list 1 "matthew" 2 3))
   most-negative-fixnum
   #c(4 3)
   #*101001010010101010101
   (- 220.0)
   most-positive-long-float))

#-sbcl (inspector-test)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (inspector-test))

;;; other ideas

;;; inspectors for:
;;; hashtable inspector
;;; readtable inspector
;;; filenames/pathnames
;;; streams
;;; ratio
;;; hex dump like thing for arrays or 2d arrays of unsigned-bytes
;;; cffi foregin pointers - hex dumps too?

;;; context menu ideas

;;; Inspect (same as double click)
;;; Set value
;;;  - same as in place edit, but takes expressions
;;; Remove key (hash-table)
