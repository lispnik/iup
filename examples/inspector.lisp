(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-controls" "im" "iup-im" "iup-plot" "closer-mop")))

(defpackage #:iup-examples.inspector
  (:use #:common-lisp)
  (:export #:inspector))

(in-package #:iup-examples.inspector)

;;; https://core.ac.uk/download/pdf/4404837.pdf

(defclass detector ()
  ((title :initarg :title :reader title)
   (test-function :initarg :test-function :reader test-function)
   (view :initarg :view :reader view)))

(defmethod print-object ((object detector) stream)
  (print-unreadable-object (object stream)
    (format stream "~A" (title object))))

(defun make-cons-detector ()
  (make-instance 'detector
                 :title "&Cons"
                 :test-function #'consp
                 :view (lambda (object)
                         (let ((handle (iup-controls:matrix :resizematrix :yes)))
                           (setf (iup:attribute handle :numlin) 2
                                 (iup:attribute handle :numcol) 2
                                 (iup:attribute handle "0:1") "Place"
                                 (iup:attribute handle "0:2") "Value"
                                 (iup:attribute handle "1:1") "CAR"
                                 (iup:attribute handle "2:1") "CDR"
                                 (iup:attribute-id handle :alignment 1) :aleft
                                 (iup:attribute-id handle :alignment 2) :aleft
                                 (iup:attribute-id handle :alignmentlin 0) :aleft)
                           (setf (iup:attribute handle "1:2") (write-to-string (car object))
                                 (iup:attribute handle "2:2") (write-to-string (cdr object)))
                           handle))))

(defun make-list-detector ()
  (make-instance 'detector
                 :title "&List"
                 :test-function #'listp
                 :view (lambda (object)
                         (let* ((length (length object))
                                (handle (iup-controls:matrix :resizematrix :yes
                                                             :numcol 2
                                                             :numlin length)))
                           (setf (iup:attribute handle "0:1") "Index"
                                 (iup:attribute handle "0:2") "Value"
                                 (iup:attribute-id handle :alignment 1) :aleft
                                 (iup:attribute-id handle :alignment 2) :aleft
                                 (iup:attribute-id handle :alignmentlin 0) :aleft)
                           (loop :for i :from 0 :below length
                                 :for l :from 1
                                 :do (setf (iup:attribute handle (format nil "~A:1" l)) i
                                           (iup:attribute handle (format nil "~A:2" l)) (write-to-string (elt object i))))
                           handle))))

(defun make-alist-detector ()
  (make-instance 'detector
                 :title "&Alist"
                 :test-function #'(lambda (object)
                                    (and (listp object)
                                         (every #'consp object)))
                 :view (lambda (object)
                         (let* ((length (length object))
                                (handle (iup-controls:matrix :resizematrix :yes
                                                             :numcol 2
                                                             :numlin length)))
                           (setf (iup:attribute handle "0:1") "Attribute"
                                 (iup:attribute handle "0:2") "Value"
                                 (iup:attribute-id handle :alignment 1) :aleft
                                 (iup:attribute-id handle :alignment 2) :aleft
                                 (iup:attribute-id handle :alignmentlin 0) :aleft)
                           (loop for ((car . cdr)) on object
                                 for l from 1
                                 do (setf (iup:attribute handle (format nil "~A:1" l)) (write-to-string car)
                                          (iup:attribute handle (format nil "~A:2" l)) (write-to-string cdr)))
                           handle))))

(defun make-plist-detector ()
  (make-instance 'detector
                 :title "&Plist"
                 :test-function #'(lambda (object)
                                    (and (listp object)
                                         (evenp (length object))))
                 :view (lambda (object)
                         (let* ((length (length object))
                                (handle (iup-controls:matrix :resizematrix :yes
                                                             :numcol 2
                                                             :numlin (/ length 2))))
                           (setf (iup:attribute handle "0:1") "Attribute"
                                 (iup:attribute handle "0:2") "Value"
                                 (iup:attribute-id handle :alignment 1) :aleft
                                 (iup:attribute-id handle :alignment 2) :aleft
                                 (iup:attribute-id handle :alignmentlin 0) :aleft)
                           (loop for (car cdr) on object by #'cddr
                                 for l from 1
                                 do (setf (iup:attribute handle (format nil "~A:1" l)) (write-to-string car)
                                          (iup:attribute handle (format nil "~A:2" l)) (write-to-string cdr)))
                           handle))))

(defun make-vector-detector ()
  (make-instance 'detector
                 :title "&Vector"
                 :test-function #'vectorp
                 :view (lambda (object)
                         (let* ((length (length object))
                                (handle (iup-controls:matrix :resizematrix :yes
                                                             :numcol 2
                                                             :numlin length)))
                           (setf (iup:attribute handle "0:1") "Index"
                                 (iup:attribute handle "0:2") "Value")
                           (loop :for i :from 0 :below length
                                 :for l :from 1
                                 :do (setf (iup:attribute handle (format nil "~A:1" l)) i
                                           (iup:attribute handle (format nil "~A:2" l)) (prin1-to-string (elt object i))))
                           handle))))

(defun make-list-plotting-detector ()
  (make-instance 'detector
                 :title "&Plot"
                 :test-function #'(lambda (object)
                                    (and (listp object) (every #'realp object)))
                 :view (lambda (object)
                         (let ((handle (iup-plot:plot)))
                           (iup-plot:with-plot (handle)
                             (loop for i from 0
                                   for e in object
                                   do (iup-plot:add handle i e)))
                           handle))))

(defun make-2d-array-detector ()
  (make-instance 'detector
                 :title "&Matrix"
                 :test-function #'(lambda (object)
                                    (and (arrayp object)
                                         (= 2 (array-rank object))
                                         (loop for i below (array-total-size object)
                                               always (realp (row-major-aref object i)))))
                 :view #'(lambda (object)
                           (let* ((numcol (elt (array-dimensions object) 1))
                                  (numlin (elt (array-dimensions object) 0))
                                  (handle (iup-controls:matrix :numcol numcol :numlin numlin)))
                             (loop for i below numlin
                                   for l = (1+ i)
                                   do (setf (iup:attribute handle (format nil "~A:0" l)) i))
                             (loop for j below numcol
                                   for c = (1+ j)
                                   do (setf (iup:attribute handle (format nil "0:~A" c)) j
                                            (iup:attribute-id handle :alignment c) :aright))
                             (loop for i below (elt (array-dimensions object) 0)
                                   for l = (1+ i)
                                   do (loop for j below (elt (array-dimensions object) 1)
                                            for c = (1+ j)
                                            do (setf (iup:attribute handle (format nil "~A:~A" l c))
                                                     (aref object i j))))
                             handle))))

(defun make-2d-array-plotting-detector ()
  (make-instance 'detector
                 :title "&Plot"
                 :test-function #'(lambda (object)
                                    (and (arrayp object)
                                         (= 2 (array-rank object))
                                         (loop for i below (array-total-size object)
                                               always (realp (row-major-aref object i)))))
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
                           (with-package-iterator (iterator object :internal :external)
                             (loop
                               (multiple-value-bind
                                     (more-p symbol accessibility package)
                                   (iterator)
                                 (if more-p
                                     (print (list more-p symbol accessibility package))
                                     (return)))))

                           (let ((handle (iup-controls:matrix :numcol 2)))

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

(defclass vec2 ()
  ((x :initarg :x :initform 0.0 :type single-float :reader x :reader a)
   (y :initarg :y :accessor y)))

(defclass pos (vec2)
  ((label :initarg :label :reader label)))

(defclass ship ()
  ((pos :type pos)
   (dir :type vec2)))

(defun make-class-slots-detector ()
  (make-instance 'detector
                 :title "&Slots"
                 :test-function #'c2cl:classp
                 :view (lambda (object)
                         (let* ((class-slots (c2mop:class-direct-slots object))
                                (handle (iup-controls:matrix :numlin (length class-slots)
                                                             :numcol 7))
                                (slot-metadata '(("Name" c2mop:slot-definition-name)
                                                 ("Type" c2mop:slot-definition-type)
                                                 ("Readers" c2mop:slot-definition-readers)
                                                 ("Writers" c2mop:slot-definition-writers)
                                                 ("Initargs" c2mop:slot-definition-initargs)
                                                 ("Initforms" c2mop:slot-definition-initform)
                                                 ("Allocation" c2mop:slot-definition-allocation))))
                           (loop for ((heading function)) on slot-metadata
                                 for c from 1
                                 do (setf (iup:attribute handle (format nil "0:~A" c)) heading))
                           (loop for slot-definition in class-slots
                                 for r from 1
                                 do (loop for ((heading function))  on slot-metadata
                                          for c from 1
                                          do (setf (iup:attribute-id handle :alignment c) :aleft
                                                   (iup:attribute handle (format nil "~A:~A" r c)) (write-to-string (funcall function slot-definition)))))
                           handle))))

(defun inspector ()
  (iup:with-iup ()
    (iup-controls:open)
    (iup-plot:open)
    (let* ((detectors (list
                       (make-cons-detector)
                       (make-list-detector)
                       (make-alist-detector)
                       (make-plist-detector)
                       (make-vector-detector)
                       (make-list-plotting-detector)
                       (make-2d-array-detector)
                       (make-2d-array-plotting-detector)
                       (make-package-detector)
                       (make-string-detector)
                       (make-class-detector)
                       (make-class-slots-detector)))
           (object
             (find-class 'ship)
             #+nil (find-package "ALEXANDRIA")
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
             #+nil (make-array 4 :initial-contents (list 1 "matthew" 2 3)))
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
           (dialog (iup:dialog vbox
                               :title "Inspector, World!")))
      (iup:show dialog)
      (iup:main-loop))))

#-sbcl (inspector)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (inspector))
