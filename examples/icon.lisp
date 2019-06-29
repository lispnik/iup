;;; Generated from org-mode, do not edit

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-im")))

(defpackage #:iup-examples.icon
  (:use #:common-lisp)
  (:export #:icon))

(in-package #:iup-examples.icon)

(defun icon ()
  (iup:with-iup ()
    (let ((icon (iup-im:load-image (asdf:system-relative-pathname "iup" "examples/lispalien.ico"))))
      (setf (iup:handle "lispalien") icon))
    (let* ((label (iup:flat-label :image "lispalien" :expand :yes))
	   (dialog (iup:dialog label :title "Icon from File"
				     :icon "lispalien"
				     :size "THIRDxTHIRD")))
      (iup:show dialog)
      (iup:main-loop))))

#-sbcl (icon)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (icon))
