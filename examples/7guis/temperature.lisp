(defpackage #:iup-examples.temperature
  (:use #:common-lisp)
  (:export #:temperature))

(in-package #:iup-examples.temperature)

(defun temperature ()
  (iup:with-iup ()
    (let* ((celsius-label (iup:label :title "Celsius = "))
	   (farenheit-label (iup:label :title "Farenheit"))
	   (celsius (iup:text :value "" :size 50 #|:mask iup:+mask-float+|#))
	   (farenheit (iup:text :value "" :size 50 #|:mask iup:+mask-float+|# ))
	   (hbox (iup:hbox
		  (list celsius celsius-label farenheit farenheit-label)
		  :alignment "ACENTER"
		  :gap 10
		  :margin "10x10"))
	   (dialog (iup:dialog hbox :title "Temperature Converter")))
      (setf (iup:callback celsius :valuechanged_cb)
	    #'(lambda (self)
		(let ((celsius (iup:attribute self :value 'single-float)))
		  (setf (iup:attribute farenheit :value)
			(format nil "~2$" (+ (* celsius 9/5) 32))))
		iup::+default+)
	    (iup:callback farenheit :valuechanged_cb)
	    #'(lambda (self)
		(let ((farenheit (iup:attribute self :value 'single-float)))
		  (setf (iup:attribute celsius :value)
			(format nil "~2$" (* (- farenheit 32 ) 5/9))))
		iup::+default+))
      (iup:show dialog)
      (iup:main-loop))))

#+nil
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (temperature))
