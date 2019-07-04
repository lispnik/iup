;;; Generated from org-mode, do not edit

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "iup"))

(defpackage #:iup-examples.idle
  (:use #:common-lisp)
  (:export #:idle))

(in-package #:iup-examples.idle)

(defun idle ()
  (iup:with-iup ()
    (let* ((counter (iup:label :fontsize 24
                               :title 0
                               :expand :yes
                               :alignment :acenter))
           (start-button (iup:button :title "&Start" :expand :horizontal))
           (stop-button (iup:button :title "S&top" :expand :horizontal))
           (do-nothing nil)
           (do-nothing-toggle (iup:toggle :title "Do nothing"
                                          :action (lambda (handle state)
                                                    (setf do-nothing (not do-nothing))
                                                    iup:+default+)))
           (vbox (iup:vbox (list counter
                                 (iup:hbox (list start-button stop-button do-nothing-toggle)
                                           :cgap 5))
                           :margin "5x5"))
           (dialog (iup:dialog vbox
                               :title (format nil "Idle Example on ~A" (lisp-implementation-type))
                               :size "QUARTERxQUARTER")))
      (setf (iup:callback start-button :action)
            (lambda (handle)
              (setf (iup:idle-action)
                    (lambda ()
                      (unless do-nothing
                        (setf (iup:attribute counter :title)
                              (1+ (iup:attribute counter :title 'number))))
                      iup:+default+))
              iup:+default+))
      (setf (iup:callback stop-button :action)
            (lambda (handle)
              (setf (iup:idle-action) nil)
              iup:+default+))
      (iup:show dialog)
      (iup:main-loop))))

#-sbcl (idle)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (idle))
