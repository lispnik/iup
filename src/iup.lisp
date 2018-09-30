(defpackage #:iup
  (:use #:common-lisp
	#:cffi)
  (:export #:iup-open
	   #:iup-close
	   #:iup-version
	   #:iup-version-number
	   #:iup-vbox))

(in-package #:iup)

(define-condition iup-error () ())

(defun iup-open ()
  (let ((ret (iup-cffi::%iup-open (cffi:null-pointer) (cffi:null-pointer))))
    (when (= ret iup-cffi::%iup-error)
      (error 'iup-error))))

(setf (fdefinition 'iup-close) #'iup-cffi:%iup-close)
(setf (fdefinition 'iup-version) #'iup-cffi::%iup-version)
(setf (fdefinition 'iup-version-number) #'iup-cffi::%iup-version-number)

(defun iup-vbox (&rest children)
  (assert (every ))
  (let ((array (foreign-alloc 'ihandle :initial-contents children :null-terminated-p t)))
    (unwind-protect
	 (iup-cffi::%iup-vbox-v array)
      (foreign-free array))))

()
(defun iup-label (&rest attributes &key (active t active-set-p) alignment (title title))
  (list attributes
	active
	alignment
	title
	active-set-p))

(iup-label :alignment 123 :title "adsf")

(setf (mousemove-cb iup-label) #'())



