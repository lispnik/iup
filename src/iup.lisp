(defpackage #:iup
  (:use #:common-lisp
	#:cffi)
  (:export #:iup-open
	   #:iup-close
	   #:iup-version
	   #:iup-version-number
	   #:iup-vbox
	   #:iup-label))

(in-package #:iup)

(define-condition iup-error () ())

(defun iup-open ()
  (let ((ret (iup-cffi::%iup-open (cffi:null-pointer) (cffi:null-pointer))))
    (when (= ret iup-cffi::%iup-error)
      (error 'iup-error))))

(setf (fdefinition 'iup-close) #'iup-cffi::%iup-close)
(setf (fdefinition 'iup-version) #'iup-cffi::%iup-version)
(setf (fdefinition 'iup-version-number) #'iup-cffi::%iup-version-number)

(defun iup-vbox (&rest children)
  (let ((array (foreign-alloc 'ihandle :initial-contents children :null-terminated-p t)))
    (unwind-protect
	 (iup-cffi::%iup-vbox-v array)
      (foreign-free array))))


(defmacro defiup-control (name attributes)
  )

(defiup-control label
    `(defun ,(intern ))
    ((:name active
      :to-converter yes-no-converter
      :from-converter yes-no-converter
      :default nil)))

()

(defun iup-label (&rest attributes &key )
  (list attributes
	active
	alignment
	title
	active-set-p))

;;; some controls have attributes that can be set at creation only e.g. IupDial
;;; some attributes are write only
;;; some attributes are read only
;;; some are not inheritable (maybe not applicable for Lisp binding?)
;;; attribute names have no spacing: ARROWIMAGEHIGHLIGHT -- convert to :arrow-image-highlight?

;;; containers are like controls but have children
;;; contrainers don't have callbacks??? nope -- some do: IupFrame
;;; some containers inherit from others (IupFlatFrame inheris from IupBackgroundBox

;;; callbacks are varied


(defclass control () ())
(defclass label (control)
  )
