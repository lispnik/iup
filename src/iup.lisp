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
	   #:hbox

	   #:label

	   #:item
	   #:submenu
	   #:menu
	   #:button
	   #:separator

	   #:with-iup
	   #:attr


	   #:config
	   #:config-load
	   #:config-save
	   #:config-dialog-show
	   #:config-dialog-closed)
  (:shadow #:open
	   #:close))

(in-package #:iup)

(defmacro alias (target source)
  `(setf (fdefinition ,target) ,source))

(defconstant +center+ #xffff)
(defconstant +left+ #xfffe)
(defconstant +right+ #xffd)
(defconstant +mousepos+ #xfffc)
(defconstant +current+ #xfffb)
(defconstant +centerparent+ #xfffa)
(defconstant +top+ +left+)
(defconstant +bottom+ +right+)

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

(defun menu (&rest children)
  (let ((array (foreign-alloc 'iup-cffi::ihandle :initial-contents children :null-terminated-p t)))
    (unwind-protect
	 (iup-cffi::%iup-menu-v array)
      (foreign-free array))))

(defun button (&rest attrs &key &allow-other-keys)
  (apply-attrs (iup-cffi::%iup-button (cffi:null-pointer) (cffi:null-pointer)) attrs))

(defun vbox (children &rest attrs &key &allow-other-keys)
  (let ((array (foreign-alloc 'iup-cffi::ihandle :initial-contents children :null-terminated-p t)))
    (unwind-protect
	 (apply-attrs (iup-cffi::%iup-vbox-v array) attrs)
      (foreign-free array))))

(defun hbox (children &rest attrs &key &allow-other-keys)
  (let ((array (foreign-alloc 'iup-cffi::ihandle :initial-contents children :null-terminated-p t)))
    (unwind-protect
	 (apply-attrs (iup-cffi::%iup-hbox-v array) attrs)
      (foreign-free array))))

(defun attr (handle attr)
  (iup-cffi::%iup-get-attribute handle (symbol-name attr)))

(defun (setf attr) (new-value handle attr)
  (if (typep new-value 'string)
      (iup-cffi::%iup-set-str-attribute handle attr (or new-value (cffi:null-pointer)))
      ;; FIXME else it's a handle??? note: in the source code for IUP
      ;; an internal function check for a is this pointer a handle by
      ;; tesing the first 4 bytes
      (iup-cffi::%iup-set-attribute-handle handle attr (or new-value (cffi:null-pointer)))))

(defun apply-attrs (handle attrs)	;TODO make into (setf attrs)?
  (loop for (attr value) on attrs by #'cddr
	do (progn
	     (setf (attr handle attr) value))
	finally (return handle)))

(defun multi-line (&rest attrs &key &allow-other-keys)
  (apply-attrs (iup-cffi::%iup-multi-line (cffi:null-pointer)) attrs))

(defun dialog (child &rest attrs &key &allow-other-keys)
  (apply-attrs (iup-cffi::%iup-dialog child) attrs))

(defun item (&rest attrs &key &allow-other-keys)
  (apply-attrs (iup-cffi::%iup-item (cffi:null-pointer) (cffi:null-pointer)) attrs))

(defun submenu (menu &rest attrs &key &allow-other-keys)
  (apply-attrs (iup-cffi::%iup-submenu (cffi:null-pointer) menu) attrs))

(alias 'separator #'iup-cffi::%iup-separator)

(defun label (&rest attrs &key &allow-other-keys)
  (apply-attrs (iup-cffi::%iup-label (cffi:null-pointer)) attrs))

;;; some controls have attrs that can be set at creation only e.g. IupDial
;;; some attrs are write only
;;; some attrs are read only
;;; some are not inheritable (maybe not applicable for Lisp binding?)
;;; attribute names have no spacing: ARROWIMAGEHIGHLIGHT -- convert to :arrow-image-highlight?

;;; containers are like controls but have children
;;; contrainers don't have callbacks??? nope -- some do: IupFrame
;;; some containers inherit from others (IupFlatFrame inheris from IupBackgroundBox

;;; callbacks are varied


(defun config (&rest attrs &key &allow-other-keys)
  (apply-attrs (iup-cffi::%iup-config) attrs))

(alias 'config-load #'iup-cffi::%iup-config-load)
(alias 'config-save #'iup-cffi::%iup-config-save)

(alias 'config-dialog-show #'iup-cffi::%iup-config-dialog-show)
(alias 'config-dialog-closed #'iup-cffi::%iup-config-dialog-closed)
