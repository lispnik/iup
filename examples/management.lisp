(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "iup"))

(defpackage #:iup-examples.management
  (:use #:common-lisp)
  (:shadow #:management)
  (:export #:management))

(in-package #:iup-examples.management)

(defvar *counter* 1)

(defun button-callback (handle)
  (let* ((dialog (iup:get-dialog handle))
         (dialog-child (iup:get-child dialog 0)))
    (iup:destroy dialog-child)
    (let ((new-dialog-child (make-widget)))
      (iup:append dialog new-dialog-child)
      (iup:map new-dialog-child)
      (iup:refresh dialog))
    iup:+default+))

(defun make-widget ()
  (let* ((label (iup:label :title (format nil "Current counter ~A" *counter*)))
         (text (iup:text :value "here an example"
                         :expand :horizontal))
         (button (iup:button :title "Replace!"
                             :action 'button-callback
                             :expand :horizontal))
         (vbox (iup:vbox (list label text button))))
    (incf *counter*)
    vbox))

(defun management ()
  (iup:with-iup ()
    (let* ((dialog (iup:dialog (make-widget) :title "Hierarchy Management Example")))
      (iup:show dialog)
      (iup:main-loop))))

#-sbcl (management)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (management))
