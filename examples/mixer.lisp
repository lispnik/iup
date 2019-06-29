;;; Generated from org-mode, do not edit

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "iup"))

(defpackage #:iup-examples.mixer
  (:use #:common-lisp)
  (:export #:mixer))

(in-package #:iup-examples.mixer)

(defun make-mixer-action (r g b button label)
  (lambda (handle)
    (declare (ignore handle))
    (let ((color (format nil "~A ~A ~A"
                         (floor (iup:attribute r :value 'number))
                         (floor (iup:attribute g :value 'number))
                         (floor (iup:attribute b :value 'number)))))
      (setf (iup:attribute button :fgcolor) color
            (iup:attribute label :title) color)
      (iup:refresh button))
    iup:+default+))

(defun mixer ()
  (iup:with-iup ()
    (let* ((button (iup:flat-button :expand :yes :canfocus :no))
           (label (iup:label :expand :horizontal :title "#x00000" :alignment "ACENTER:ACENTER"))
           (r (iup:val :expand :horizontal :min 0 :max 255))
           (g (iup:val :expand :horizontal :min 0 :max 255))
           (b (iup:val :expand :horizontal :min 0 :max 255))
           (vbox (iup:vbox
                  (list (iup:grid-box
                         (list (iup:label :title "&Red")   r
                               (iup:label :title "&Green") g
                               (iup:label :title "&Blue")  b)
                         :numdiv 2
                         :cgapcol 10
                         :cgaplin 5)
                        button
                        label)
                  :cmargin 5
                  :cgap 5
                  :margin "x5"))
           (dialog (iup:dialog vbox :title "Color Mixer Example" :size "QUARTERxQUARTER")))
      (loop :with action := (make-mixer-action r g b button label)
            :for handle :in (list r g b)
            :do (setf (iup:callback handle :valuechanged_cb)  action))
      (iup:show dialog)
      (iup:main-loop))))

#-sbcl (mixer)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (mixer))
