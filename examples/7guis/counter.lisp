(defpackage #:iup-examples.counter
  (:use #:common-lisp)
  (:export #:counter))

(in-package #:iup-examples.counter)

(defun counter ()
  (iup:with-iup ()
    (let* ((count (iup:text :readonly "YES" :value 0 :size 60))
	   (button (iup:button :title "Count"
			       :size 60
			       :action #'(lambda (handle)
					   (declare (ignore handle))
					   (setf (iup:attribute count :value)
						 (1+ (iup:attribute count :value :int))))))
	   (hbox (iup:hbox (list count button) :gap 10 :margin "10x10"))
	   (dialog (iup:dialog hbox :title "Counter")))
      (iup:show dialog)
      (iup:main-loop))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (counter))
