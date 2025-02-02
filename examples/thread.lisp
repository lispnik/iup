(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "bordeaux-threads")))

(defpackage #:iup-examples.thread
  (:use #:common-lisp)
  (:export #:thread))

(in-package #:iup-examples.thread)

(defun worker-function ()
  (loop for i from 0 below 100
	do (progn
	     (sleep 5)
	     (format t "worker function~%"))))

(defun button-callback (handle)
  (declare (ignore handle))
  (bt:make-thread 'worker-function)
  iup:+default+)

(defun thread ()
  (iup:with-iup ()
    (let* ((label (iup:label :title "Start second thread:"))
	   (button (iup:button :title "Start" :action 'button-callback))
           (vbox (iup:vbox (list label button) :alignment "ACENTER" :gap "10" :margin "10x10"))
           (dialog (iup:dialog vbox :title "thread test" :title "thread test")))
      (iup:show dialog)
      (iup:main-loop))))

#-sbcl (thread)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (thread))
