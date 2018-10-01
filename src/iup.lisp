(defpackage #:iup
  (:use #:common-lisp
	#:cffi)
  (:export #:open
	   #:close
	   #:version
	   #:version-number
	   #:vbox
	   #:label)
  (:shadow #:open
	   #:close))

(in-package #:iup)

(define-condition iup-error () ())

(defun open ()
  (let ((ret (iup-cffi::%iup-open (cffi:null-pointer) (cffi:null-pointer))))
    (when (= ret iup-cffi::%iup-error)
      (error 'iup-error))))

(setf (fdefinition 'close) #'iup-cffi::%iup-close)
(setf (fdefinition 'version) #'iup-cffi::%iup-version)
(setf (fdefinition 'version-number) #'iup-cffi::%iup-version-number)

(defun vbox (&rest children)
  (let ((array (foreign-alloc 'ihandle :initial-contents children :null-terminated-p t)))
    (unwind-protect
	 (iup-cffi::%iup-vbox-v array)
      (foreign-free array))))

(defun iup-label (&rest attributes &key &allow-other-keys)
  (iup-cffi::%iup-label (cffi:null-pointer))

)

;;; some controls have attributes that can be set at creation only e.g. IupDial
;;; some attributes are write only
;;; some attributes are read only
;;; some are not inheritable (maybe not applicable for Lisp binding?)
;;; attribute names have no spacing: ARROWIMAGEHIGHLIGHT -- convert to :arrow-image-highlight?

;;; containers are like controls but have children
;;; contrainers don't have callbacks??? nope -- some do: IupFrame
;;; some containers inherit from others (IupFlatFrame inheris from IupBackgroundBox

;;; callbacks are varied

