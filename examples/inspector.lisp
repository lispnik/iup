(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-controls")))

(defpackage #:iup-examples.inspector
  (:use #:common-lisp)
  (:export #:inspector))

(in-package #:iup-examples.inspector)

(defclass identifier ()
  ((title :initarg :title :reader title)
   (test-function :initarg :test-function :reader test-function)
   (view :initarg :view :reader view)))

(defmethod print-object ((object identifier) stream)
  (print-unreadable-object (object stream)
    (format stream "~A" (title object))))

(defun make-cons-identifier ()
  (make-instance 'identifier
                 :title "&Cons"
                 :test-function #'consp
                 :view (lambda (object)
                         (let ((handle (iup-controls:matrix)))
                           (setf (iup:attribute handle :numlin) 2
                                 (iup:attribute handle :numcol) 2
                                 (iup:attribute handle "0:1") "Place"
                                 (iup:attribute handle "0:2") "Value")
                           (setf (iup:attribute handle "1:1") "CAR"
                                 (iup:attribute handle "2:1") "CDR")
                           (setf (iup:attribute handle "1:2")
                                 (princ-to-string (car object))
                                 (iup:attribute handle "2:2")
                                 (princ-to-string (cdr object)))
                           handle))))

(defun make-list-identifier ()
  (make-instance 'identifier
                 :title "&List"
                 :test-function #'listp
                 :view (lambda (object)
                         (let* ((length (length object))
                                (handle (iup-controls:matrix :numcol 2 :numlin length)))
                           (setf (iup:attribute handle "0:1") "Index"
                                 (iup:attribute handle "0:2") "Value")
                           (loop :for i :from 0 :below length
                                 :for l :from 1
                                 :do (setf (iup:attribute handle (format nil "~A:1" l)) i
                                           (iup:attribute handle (format nil "~A:2" l)) (elt object i)))
                           handle))))

(defun make-vector-identifier ()
  (make-instance 'identifier
                 :title "&Vector"
                 :test-function #'vectorp
                 :view (lambda (object)
                         (let* ((length (length object))
                                (handle (iup-controls:matrix :numcol 2 :numlin length)))
                           (setf (iup:attribute handle "0:1") "Index"
                                 (iup:attribute handle "0:2") "Value")
                           (loop :for i :from 0 :below length
                                 :for l :from 1
                                 :do (setf (iup:attribute handle (format nil "~A:1" l)) i
                                           (iup:attribute handle (format nil "~A:2" l)) (prin1-to-string (elt object i))))
                           handle))))

(defparameter *indentifiers*
  (list (make-cons-identifier)
        (make-list-identifier)
        (make-vector-identifier)))

(defun inspector ()
  (iup:with-iup ()
    (iup-controls:open)
    (let* ((identifiers (list
                         (make-cons-identifier)
                         (make-list-identifier)
                         (make-vector-identifier)))
           (object '(1.0 2 #C (4 5.0)) #+nil (make-array 4 :initial-contents (list 1 "matthew" 2 3)))
           (applicable-identifiers (print (remove-if-not #'(lambda (identifier)
                                                       (funcall (test-function identifier) object))
                                                   identifiers)))
           (views (mapcar #'(lambda (identifier)
                              (funcall (view identifier) object))
                          applicable-identifiers))
           (dialog (iup:dialog (loop with tabs = (iup:tabs views)
                                     for identifier in applicable-identifiers
                                     for i from 0
                                     do (setf (iup:attribute-id tabs :tabtitle i) (title identifier))
                                     finally (return tabs))
                               :title "Inspector, World!")))
      (iup:show dialog)
      (iup:main-loop))))

#-sbcl (inspector)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (inspector))
