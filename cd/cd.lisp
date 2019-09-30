(defpackage #:iup-cd
  (:use #:common-lisp
        #:serapeum)
  (:export #:context-iup
	   #:context-iup-dbuffer
	   #:context-iup-dbuffer-rgb
	   #:context-iup-draw))

(in-package #:iup-cd)

(defalias context-iup #'iup-cd-cffi::%cd-context-iup)
(defalias context-iup-dbuffer #'iup-cd-cffi::%cd-context-iup-dbuffer)
(defalias context-iup-dbuffer-rgb #'iup-cd-cffi::%cd-context-iup-dbuffer-rgb)
(defalias context-iup-draw #'iup-cd-cffi::%cd-context-iup-draw)
