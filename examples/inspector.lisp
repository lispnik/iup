(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-controls" "im" "iup-im" "iup-plot")))

(defpackage #:iup-examples.inspector
  (:use #:common-lisp)
  (:export #:inspector))

(in-package #:iup-examples.inspector)

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
                                 (iup:attribute-id handle :alignment 1) :aleft
                                 (iup:attribute-id handle :alignment 2) :aleft
                                 (iup:attribute-id handle :alignmentlin 0) :aleft)
                           (setf (iup:attribute handle "1:1") "CAR"
                                 (iup:attribute handle "2:1") "CDR")
                           (setf (iup:attribute handle "1:2")
                                 (princ-to-string (car object))
                                 (iup:attribute handle "2:2")
                                 (write-to-string (cdr object) :pretty nil))
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
                                           (iup:attribute handle (format nil "~A:2" l)) (elt object i)))
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
                           ;; (iup-plot:with-plot (handle)
                           ;;   (loop for i from 0
                           ;;         for e in object
                           ;;         do (iup-plot:add handle i e)))
                           handle))))
(defun inspector ()
  (iup:with-iup ()
    (iup-controls:open)
    (iup-plot:open)
    (let* ((detectors (list
                       (make-cons-detector)
                       (make-list-detector)
                       (make-vector-detector)
                       (make-list-plotting-detector)
                       ))
           (object (loop for i from 0 below (* 2 pi) by 0.1
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
           (dialog (iup:dialog (loop with tabs = (iup:tabs views)
                                     for detector in applicable-detectors
                                     for i from 0
                                     do (setf (iup:attribute-id tabs :tabtitle i) (title detector))
                                     finally (return tabs))
                               :title "Inspector, World!")))
      (iup:show dialog)
      (iup:main-loop))))

#-sbcl (inspector)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (inspector))
