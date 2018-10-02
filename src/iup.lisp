(defpackage #:iup
  (:use #:common-lisp
	#:cffi)
  (:export #:+center+
	   #:+left+
	   #:+right+
	   #:+mousepos+
	   #:+current+
	   #:+centerparent+
	   #:+top+
	   #:+bottom+
	   #:open
	   #:close
	   #:main-loop
	   #:image-lib-open
	   #:version
	   #:version-number
	   #:show-xy
	   #:vbox
	   #:label

	   #:with-iup
	   #:attr)
  (:shadow #:open
	   #:close))

(in-package #:iup)

(defmacro alias (target source)
  `(setf (fdefinition ,target) ,source))

(defconstant +center+ #xffff)
(defconstant +left+ #xffff)
(defconstant +right+ #xffff)
(defconstant +mousepos+ #xffff)
(defconstant +current+ #xffff)
(defconstant +centerparent+ #xffff)
(defconstant +top+ #xffff)
(defconstant +bottom+ #xffff)

(defun open ()
  (let ((ret (iup-cffi::%iup-open (cffi:null-pointer) (cffi:null-pointer))))
    (when (= ret iup-cffi::%iup-error)
      (error 'iup-error))))

(alias 'close #'iup-cffi::%iup-close)
(alias 'main-loop #'iup-cffi::%iup-main-loop)

(defmacro with-iup (&body body)
  `(progn
     (iup:open)
     (unwind-protect
	 (progn
	   ,@body)
       (iup:close))))

(alias 'image-lib-open #'iup-cffi::%iup-image-lib-open)
(alias 'version #'iup-cffi::%iup-version)
(alias 'version-number #'iup-cffi::%iup-version-number)

(alias 'show-xy #'iup-cffi::%iup-show-xy)

(defun vbox (&rest children)
  (let ((array (foreign-alloc 'iup-cffi::ihandle :initial-contents children :null-terminated-p t)))
    (unwind-protect
	 (iup-cffi::%iup-vbox-v array)
      (foreign-free array))))

(defun attr (handle attr)
  (iup-cffi::%iup-get-attribute handle (symbol-name attr)))

(defun (setf attr) (new-value handle attr)
  (iup-cffi::%iup-set-str-attribute handle (symbol-name attr) (or new-value (cffi:null-pointer))))

(defun apply-attributes (handle attributes)
  (loop for (key value) on attributes by #'cddr
	do (iup-cffi::%iup-set-str-attribute
	    handle
	    (symbol-name key)
	    (or value (cffi:null-pointer)))))

(defun multi-line (&rest attributes &key &allow-other-keys)
  (let ((handle (iup-cffi::%iup-multi-line (cffi:null-pointer))))
    (apply-attributes handle attributes)
    handle))

(defun dialog (child &rest attributes &key &allow-other-keys)
  (let ((handle (iup-cffi::%iup-dialog child)))
    (apply-attributes handle attributes)
    handle))

;;; some controls have attributes that can be set at creation only e.g. IupDial
;;; some attributes are write only
;;; some attributes are read only
;;; some are not inheritable (maybe not applicable for Lisp binding?)
;;; attribute names have no spacing: ARROWIMAGEHIGHLIGHT -- convert to :arrow-image-highlight?

;;; containers are like controls but have children
;;; contrainers don't have callbacks??? nope -- some do: IupFrame
;;; some containers inherit from others (IupFlatFrame inheris from IupBackgroundBox

;;; callbacks are varied

