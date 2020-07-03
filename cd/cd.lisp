(defpackage #:iup-cd
  (:use #:common-lisp)
  (:export #:context-iup
	   #:context-iup-dbuffer
	   #:context-iup-dbuffer-rgb
	   #:context-iup-draw))

(in-package #:iup-cd)

(setf (fdefinition 'context-iup) #'iup-cd-cffi::%cd-context-iup)
(setf (fdefinition 'context-iup-dbuffer) #'iup-cd-cffi::%cd-context-iup-dbuffer)
(setf (fdefinition 'context-iup-dbuffer-rgb) #'iup-cd-cffi::%cd-context-iup-dbuffer-rgb)
(setf (fdefinition 'context-iup-draw) #'iup-cd-cffi::%cd-context-iup-draw)
