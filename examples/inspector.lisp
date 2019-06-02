(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-controls" "im" "iup-im" "iup-plot" "closer-mop" "cl-prime-maker" "serapeum")))

(defpackage #:iup-examples.inspector
  (:use #:common-lisp)
  (:export #:inspector))

(in-package #:iup-examples.inspector)

;;; https://core.ac.uk/download/pdf/4404837.pdf

(defclass detector ()
  ((title :initarg :title :reader title)
   (test-function :initarg :test-function :reader test-function)
   (view :initarg :view :reader view)))

(defun create-matrix (&rest rest &key headers (column-default-alignment :aleft) &allow-other-keys)
  (let ((matrix-ex-args (copy-list rest)))
    (remf matrix-ex-args :headers)
    (remf matrix-ex-args :column-default-alignment)
    (let* ((handle (apply #'iup-controls:matrix-ex matrix-ex-args)))
      (setf (iup:attribute-id handle :alignmentlin 0) :aleft
            (iup:attribute handle :resizematrix) :yes
            (iup:attribute handle :numcol_visible_last) :yes)
      (loop for header in headers
            for c from 1
            do (setf
                (iup:attribute-id handle :alignment c) column-default-alignment
                (iup:attribute-id-2 handle nil 0 c) header))
      handle)))

(defun make-cons-detector ()
  (make-instance 'detector
                 :title "&Cons"
                 :test-function #'consp
                 :view (lambda (object)
                         (let ((handle (create-matrix :numlin 2 :numcol 2 :headers '("Place" "Value"))))
                           (setf (iup:attribute-id-2 handle nil 1 1) "CAR"
                                 (iup:attribute-id-2 handle nil 2 1) "CDR"
                                 (iup:attribute-id handle :alignment 1) :aleft
                                 (iup:attribute-id handle :alignment 2) :aleft)
                           (setf (iup:attribute-id-2 handle nil 1 2) (write-to-string (car object))
                                 (iup:attribute-id-2 handle nil 2 2) (write-to-string (cdr object)))
                           handle))))

(defun make-list-detector ()
  (make-instance 'detector
                 :title "&List"
                 :test-function #'listp
                 :view (lambda (object)
                         (let* ((length (length object))
                                (handle (create-matrix :numcol 2 :numlin length :headers '("Index" "Value"))))
                           (setf (iup:attribute-id handle :alignment 1) :aleft
                                 (iup:attribute-id handle :alignment 2) :aleft)
                           (loop :for i :from 0 :below length
                                 :for l :from 1
                                 :do (setf (iup:attribute-id-2 handle nil l 1) i
                                           (iup:attribute-id-2 handle nil l 2) (write-to-string (elt object i))))
                           handle))))

(defun make-alist-detector ()
  (make-instance 'detector
                 :title "&Alist"
                 :test-function #'(lambda (object)
                                    (and (listp object)
                                         (every #'consp object)))
                 :view (lambda (object)
                         (let* ((length (length object))
                                (handle (create-matrix :numcol 2 :numlin length :headers '("Attribute" "Value"))))
                           (setf (iup:attribute-id handle :alignment 1) :aleft
                                 (iup:attribute-id handle :alignment 2) :aleft)
                           (loop :for ((car . cdr)) :on object
                                 :for l :from 1
                                 :do (setf (iup:attribute-id-2 handle nil l 1) (write-to-string car)
                                           (iup:attribute-id-2 handle nil l 2) (write-to-string cdr)))
                           handle))))

(defun make-plist-detector ()
  (make-instance 'detector
                 :title "&Plist"
                 :test-function #'(lambda (object)
                                    (and (listp object)
                                         (evenp (length object))))
                 :view (lambda (object)
                         (let* ((length (length object))
                                (handle (create-matrix :numcol 2 :numlin (/ length 2) :headers '("Attribute" "Value"))))
                           (setf (iup:attribute-id handle :alignment 1) :aleft
                                 (iup:attribute-id handle :alignment 2) :aleft)
                           (loop :for (car cdr) :on object :by #'cddr
                                 :for l :from 1
                                 :do (setf (iup:attribute-id-2 handle nil l 1) (write-to-string car)
                                           (iup:attribute-id-2 handle nil l 2) (write-to-string cdr)))
                           handle))))

(defun make-vector-detector ()
  (make-instance 'detector
                 :title "&Vector"
                 :test-function #'vectorp
                 :view (lambda (object)
                         (let* ((length (length object))
                                (handle (create-matrix :numcol 2 :numlin length :headers '("Index" "Value"))))
                           (loop :for i :from 0 :below length
                                 :for l :from 1
                                 :for e :across object
                                 :do (setf (iup:attribute-id-2 handle nil l 1) i
                                           (iup:attribute-id-2 handle nil l 2) (write-to-string e)))
                           handle))))

(defun make-list-plotting-detector ()
  (make-instance 'detector
                 :title "&Plot"
                 :test-function #'(lambda (object)
                                    (and (listp object) (every #'realp object)))
                 :view (lambda (object)
                         (let ((handle (iup-plot:plot)))
                           (iup-plot:with-plot (handle)
                             (loop :for i :from 0
                                   :for e :in object
                                   :do (iup-plot:add handle i e)))
                           handle))))

(defun make-2d-array-detector ()
  (make-instance 'detector
                 :title "&Array"
                 :test-function #'(lambda (object)
                                    (and (arrayp object)
                                         (= 2 (array-rank object))
                                         (loop for i below (array-total-size object)
                                               always (realp (row-major-aref object i)))))
                 :view #'(lambda (object)
                           (let* ((numcol (elt (array-dimensions object) 1))
                                  (numlin (elt (array-dimensions object) 0))
                                  (handle (create-matrix :numcol numcol :numlin numlin)))
                             (loop :for i :below numlin
                                   :for l = (1+ i)
                                   :do (setf (iup:attribute-id-2 handle nil l 0) i))
                             (loop :for j :below numcol
                                   :for c = (1+ j)
                                   :do (setf (iup:attribute-id-2 handle nil 0 c) j
                                             (iup:attribute-id handle :alignment c) :aright))
                             (loop :for i :below (elt (array-dimensions object) 0)
                                   :for l = (1+ i)
                                   :do (loop :for j :below (elt (array-dimensions object) 1)
                                             :for c = (1+ j)
                                             :do (setf (iup:attribute-id-2 handle nil l c) (aref object i j))))
                             handle))))

(defun make-2d-array-plotting-detector ()
  (make-instance 'detector
                 :title "&Plot"
                 :test-function #'(lambda (object)
                                    (and (arrayp object)
                                         (= 2 (array-rank object))
                                         (loop :for i :below (array-total-size object)
                                               :always (realp (row-major-aref object i)))))
                 :view #'(lambda (object)
                           (let ((handle (iup-controls:cells
                                          :ncols_cb (lambda (handle) (elt (array-dimensions object) 1))
                                          :nlines_cb (lambda (handle) (elt (array-dimensions object) 0))
                                          :draw_cb (lambda (handle i j xmin xmax ymin ymax canvas)
                                                     ;; FIXME put a nice gradient scale here
                                                     (ecase (random 2)
                                                       (1 (setf (cd:foreground canvas) cd:+white+))
                                                       (0 (setf (cd:foreground canvas) cd:+blue+)))
                                                     (cd:box canvas xmin xmax ymin ymax)
                                                     iup:+default+)
                                          :mouseclick_cb (lambda (handle button pressed line column x y status)
                                                           (iup:message (format nil "Value at ~A, ~A" (1- line) (1- column))
                                                                        (write-to-string (aref object (1- line) (1- column))))
                                                           iup:+default+))))
                             handle))))

(defun make-package-detector ()
  (make-instance 'detector
                 :title "&Package"
                 :test-function #'packagep
                 :view #'(lambda (object)
                           (let ((handle
                                   (create-matrix :numcol 2 :numlin 4 :headers '("Attribute" "Value")
                                                  :map_cb #'(lambda (handle)
                                                              (setf (iup:attribute-id-2 handle nil 1 2) (write-to-string (package-name object))
                                                                    (iup:attribute-id-2 handle nil 2 2) (write-to-string (package-nicknames object))
                                                                    (iup:attribute-id-2 handle nil 3 2) (write-to-string (package-use-list object))
                                                                    (iup:attribute-id-2 handle nil 4 2) (write-to-string (package-used-by-list object)))
                                                              iup:+default+))))
                             (setf (iup:attribute-id-2 handle nil 1 1) "Name"
                                   (iup:attribute-id-2 handle nil 2 1) "Nicknames"
                                   (iup:attribute-id-2 handle nil 3 1) "Uses"
                                   (iup:attribute-id-2 handle nil 4 1) "Used by")
                             handle))))

(defun format-engineering (stream x)
  (let* ((base (log (abs x) 10))
         (scale (1+ (round (mod base 3))))
         (format-string (format nil "~~,,,~DE" scale)))
    (format stream format-string x)))

(defun make-float-detector ()
  (make-instance 'detector
                 :title "&Float"
                 :test-function #'floatp
                 :view #'(lambda (object)
                           (let ((handle (create-matrix :numcol 2 :numlin 8 :headers '("Attribute" "Value"))))
                             (multiple-value-bind
                                   (significand exponent sign)
                                 (decode-float object)
                               (loop for label in '("Type" "Scientific" "Engineering" "Sign" "Significand" "Exponent" "Precision" "Digits")
                                     for r from 1
                                     do (setf (iup:attribute-id-2 handle nil r 1) label))
                               (setf (iup:attribute-id-2 handle nil 1 2) (write-to-string (type-of object))
                                     (iup:attribute-id-2 handle nil 2 2) (princ-to-string (format nil "~E" object))
                                     (iup:attribute-id-2 handle nil 3 2) (princ-to-string (format-engineering nil object))
                                     (iup:attribute-id-2 handle nil 4 2) sign
                                     (iup:attribute-id-2 handle nil 5 2) significand
                                     (iup:attribute-id-2 handle nil 6 2) exponent
                                     (iup:attribute-id-2 handle nil 7 2) (float-precision object)
                                     (iup:attribute-id-2 handle nil 8 2) (float-digits object)))
                             handle))))

(defun make-package-symbols-detector ()
  (make-instance 'detector
                 :title "&Symbols"
                 :test-function #'packagep
                 :view #'(lambda (object)
                           (let ((handle
                                   (create-matrix :numcol 3
                                                  :headers '("Symbol" "Accessibility" "Package")
                                                  :map_cb (lambda (handle)
                                                            (with-package-iterator (iterator object :external :internal :inherited)
                                                              (loop for r from 1
                                                                    do (multiple-value-bind
                                                                             (more-p symbol accessibility package)
                                                                           (iterator)
                                                                         (if more-p
                                                                             (progn
                                                                               (setf (iup:attribute handle :addlin) r)
                                                                               (setf (iup:attribute-id-2 handle nil r 1) (symbol-name symbol)
                                                                                     (iup:attribute-id-2 handle nil r 3) (package-name package)
                                                                                     (iup:attribute-id-2 handle nil r 2) (write-to-string accessibility)))
                                                                             (return)))))
                                                            iup:+default+))))
                             (setf (iup:attribute-id handle :alignment 1) :aleft
                                   (iup:attribute-id handle :alignment 2) :aleft
                                   (iup:attribute-id handle :alignment 3) :aleft)
                             handle))))

(defun make-string-detector ()
  (make-instance 'detector
                 :title "&String"
                 :test-function #'stringp
                 :view (lambda (object)
                         (iup:label :title "TBD"))))

(defun make-class-detector ()
  (make-instance 'detector
                 :title "&Class"
                 :test-function #'c2cl:classp
                 :view (lambda (object)
                         (iup:label :title "TBD"))))

(defun make-class-precedence-list-detector ()
  (make-instance 'detector
                 :title "&Precendence"
                 :test-function #'c2cl:classp
                 :view (lambda (object)
                         (let* ((precedence-list (c2mop:class-precedence-list (find-class 'c2mop:slot-definition)))
                                (handle (create-matrix :numcol 1 :numlin (length precedence-list))))
                           (setf (iup:attribute-id-2 handle nil 0 1) "Class"
                                 (iup:attribute handle :fittotext) "C1")
                           (loop for class in precedence-list
                                 for r from 1
                                 do (setf (iup:attribute-id-2 handle nil r 1) (write-to-string class)
                                          (iup:attribute-id handle :alignment r) :aleft))
                           handle))))

(defun make-class-slots-detector ()
  (make-instance 'detector
                 :title "&Slots"
                 :test-function #'c2cl:classp
                 :view (lambda (object)
                         (let* ((class-slots (c2mop:class-direct-slots object))
                                (handle (create-matrix :numcol 7 :numlin (length class-slots)))
                                (slot-metadata '(("Name" c2mop:slot-definition-name)
                                                 ("Type" c2mop:slot-definition-type)
                                                 ("Readers" c2mop:slot-definition-readers)
                                                 ("Writers" c2mop:slot-definition-writers)
                                                 ("Initargs" c2mop:slot-definition-initargs)
                                                 ("Initform" c2mop:slot-definition-initform)
                                                 ("Allocation" c2mop:slot-definition-allocation))))
                           (loop for ((heading function)) on slot-metadata
                                 for c from 1
                                 do (setf (iup:attribute-id-2 handle nil 0 c) heading))
                           (loop for slot-definition in class-slots
                                 for r from 1
                                 do (loop for ((heading function)) on slot-metadata
                                          for c from 1
                                          do (setf (iup:attribute-id handle :alignment c) :aleft
                                                   (iup:attribute-id-2 handle nil r c) (write-to-string (funcall function slot-definition)))))
                           handle))))

(defun make-integer-detector ()
  (make-instance 'detector
                 :title "&Integer"
                 :test-function #'integerp
                 :view (lambda (object)
                         (let* ((handle (create-matrix :numcol 2
                                                       :numlin 5
                                                       :headers '("Attribute" "Value")
                                                       :map_cb #'(lambda (handle)
                                                                   (when (and (plusp object) (serapeum:fixnump object))
                                                                     (setf (iup:attribute handle :addlin) 6
                                                                           (iup:attribute-id-2 handle nil 6 1) "Prime?"
                                                                           (iup:attribute-id-2 handle nil 6 2) (write-to-string (cl-prime-maker:primep object))))
                                                                   iup:+default+))))
                           (setf (iup:attribute-id handle :alignment 2) :aright
                                 (iup:attribute-id-2 handle nil 1 1) "Binary"
                                 (iup:attribute-id-2 handle nil 2 1) "Octal"
                                 (iup:attribute-id-2 handle nil 3 1) "Decimal"
                                 (iup:attribute-id-2 handle nil 4 1) "Hexidecimal"
                                 (iup:attribute-id-2 handle nil 5 1) "Length"
                                 (iup:attribute-id-2 handle nil 1 2) (write-to-string object :radix t :base 2)
                                 (iup:attribute-id-2 handle nil 2 2) (write-to-string object :radix t :base 8)
                                 (iup:attribute-id-2 handle nil 3 2) (write-to-string object :base 10)
                                 (iup:attribute-id-2 handle nil 4 2) (write-to-string object :radix t :base 16)
                                 (iup:attribute-id-2 handle nil 5 2) (write-to-string (integer-length object)))
                           handle))))

(defun make-complex-detector ()
  (make-instance 'detector
                 :title "&Complex"
                 :test-function #'complexp
                 :view (lambda (object)
                         (let* ((handle (create-matrix :numcol 2 :numlin 5 :headers '("Component" "Value"))))
                           (setf (iup:attribute-id handle :alignment 2) :aright
                                 (iup:attribute-id-2 handle nil 1 1) "Real"
                                 (iup:attribute-id-2 handle nil 2 1) "Imaginary"
                                 (iup:attribute-id-2 handle nil 3 1) "Magnitude)"
                                 (iup:attribute-id-2 handle nil 4 1) "Phase (radian)"
                                 (iup:attribute-id-2 handle nil 5 1) "Phase (degree)"
                                 (iup:attribute-id-2 handle nil 1 2) (realpart object)
                                 (iup:attribute-id-2 handle nil 2 2) (imagpart object)
                                 (iup:attribute-id-2 handle nil 3 2) (abs object)
                                 (iup:attribute-id-2 handle nil 4 2) (phase object)
                                 (iup:attribute-id-2 handle nil 5 2) (* (/ 180 pi) (phase object)))
                           handle))))
(defun inspector ()
  (iup:with-iup ()
    (iup-controls:open)
    (iup-plot:open)
    (let* ((detectors (list
                       (make-integer-detector)
                       (make-complex-detector)
                       (make-float-detector)
                       (make-cons-detector)
                       (make-list-detector)
                       (make-alist-detector)
                       (make-plist-detector)
                       (make-vector-detector)
                       (make-list-plotting-detector)
                       (make-2d-array-detector)
                       (make-2d-array-plotting-detector)
                       (make-package-detector)
                       (make-package-symbols-detector)
                       (make-string-detector)
                       (make-class-detector)
                       (make-class-slots-detector)
                       (make-class-precedence-list-detector)))
           (object
             #+nil (find-class 'c2mop:slot-definition)
             #+nil (find-package "IUP")
             #+nil '(:foo "bar" :baz 43)
             #+nil '(("foo" . "bar")
                     ("baz" 42)
                     ("quux")
                     (:key . :value))
             #+nil (let ((array #2A ((1 2 3 3)
                                     (1 2 3 3)
                                     (1 2 3 3)
                                     (1 2 3 3))))
                     (loop for i below (array-total-size array)
                           do (setf (row-major-aref array i) (* (row-major-aref array i) (random 1.0)))
                           finally (return array)))
             #+nil (loop for i from 0 below (* 2 pi) by 0.1
                         collect (cos i))
             #+nil (iup-im:load-image (asdf:system-relative-pathname "iup" "examples/lispalien.ico"))
             #+nil *features*
             #+nil '(1.0 2 #C (4 5.0))
             #+nil (make-array 4 :initial-contents (list 1 "matthew" 2 3))
             #+nil most-negative-fixnum
             ;;             #c(4 3)
             #+nil (- 220.0)
             most-positive-long-float
             )
           (applicable-detectors (print (remove-if-not #'(lambda (detector)
                                                           (funcall (test-function detector) object))
                                                       detectors)))
           (views (mapcar #'(lambda (detector)
                              (funcall (view detector) object))
                          applicable-detectors))
           (vbox (iup:vbox (list (loop with tabs = (iup:tabs views)
                                       for detector in applicable-detectors
                                       for i from 0
                                       do (setf (iup:attribute-id tabs :tabtitle i) (title detector))
                                       finally (return tabs))
                                 (iup:label :title (format nil "Inspecting ~S" object)
                                            :expand :horizontal))))
           (dialog (iup:dialog vbox :title "Inspector, World!")))
      (iup:show dialog)
      (iup:main-loop))))

#-sbcl (inspector)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (inspector))
