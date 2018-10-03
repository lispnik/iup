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
	   #:image-lib-open
	   
	   #:main-loop
	   #:loop-step
	   #:loop-step-wait
	   #:main-loop-level
	   #:flush
	   #:exit-loop

	   #:record-input
	   #:play-input

	   #:update
	   #:update-children
	   #:redraw
	   #:refresh
	   #:refresh-children

	   #:load
	   #:load-buffer
	   
	   #:version
	   #:version-date
	   #:version-number

	   #:set-language
	   #:get-language
	   #:set-language-string
	   #:store-language-string
	   #:get-language-string
	   #:set-language-pack

	   #:destroy
	   #:detach
	   #:append
	   #:insert
	   #:get-child
	   #:get-child-pos
	   #:get-child-count
	   #:get-next-child
	   #:get-brother
	   #:get-parent
	   #:get-dialog
	   #:get-dialog-child
	   #:reparent

	   #:popup
	   #:show
	   #:show-xy
	   #:hide
	   #:map
	   #:unmap

	   #:reset-attribute
	   #:get-all-attributes
	   
	   #:vbox
	   #:hbox

	   #:label

	   #:item
	   #:submenu
	   #:menu
	   #:button
	   #:separator


	   #:config
	   #:config-load
	   #:config-save
	   #:config-dialog-show
	   #:config-dialog-closed

	   #:with-iup
	   #:attr)
  (:shadow #:open
	   #:close))

(in-package #:iup)

(defmacro alias (target source) `(setf (fdefinition ,target) ,source))

(defun attr (handle attr)
  (iup-cffi::%iup-get-attribute handle (symbol-name attr)))

(defun (setf attr) (new-value handle attr)
  (prog1
      handle
    (typecase new-value
      (string (iup-cffi::%iup-set-str-attribute handle attr (or new-value (cffi:null-pointer))))
      (symbol (iup-cffi::%iup-set-callback handle attr (cffi:get-callback new-value)))
      (t (iup-cffi::%iup-set-attribute-handle handle attr (or new-value (cffi:null-pointer)))))))

(defun (setf attrs) (attrs handle)
  (loop for (attr value) on attrs by #'cddr
	do (progn
	     (setf (attr handle attr) value))
	finally (return handle)))

(defmacro defattrfun (name args &rest body)
  `(defun ,name (,@args &rest attrs &key &allow-other-keys)
     (setf (attrs (progn ,@body)) attrs)))

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

(alias 'image-lib-open #'iup-cffi::%iup-image-lib-open)
(alias 'version #'iup-cffi::%iup-version)
(alias 'version-number #'iup-cffi::%iup-version-number)
(alias 'show-xy #'iup-cffi::%iup-show-xy)

(defattrfun menu (children)
  (let ((array (foreign-alloc 'iup-cffi::ihandle :initial-contents children :null-terminated-p t)))
    (unwind-protect
	 (iup-cffi::%iup-menu-v array)
      (foreign-free array))))

(defattrfun button () (iup-cffi::%iup-button (cffi:null-pointer) (cffi:null-pointer)))

(defattrfun vbox (children)
  (let ((array (foreign-alloc 'iup-cffi::ihandle :initial-contents children :null-terminated-p t)))
    (unwind-protect
	 (iup-cffi::%iup-vbox-v array)
      (foreign-free array))))

(defattrfun hbox (children)
  (let ((array (foreign-alloc 'iup-cffi::ihandle :initial-contents children :null-terminated-p t)))
    (unwind-protect
	 (iup-cffi::%iup-hbox-v array)
      (foreign-free array))))

(defattrfun multi-line () (iup-cffi::%iup-multi-line (cffi:null-pointer)))
(defattrfun dialog (child) (iup-cffi::%iup-dialog child))
(defattrfun item () (iup-cffi::%iup-item (cffi:null-pointer) (cffi:null-pointer)))
(defattrfun submenu (menu) (iup-cffi::%iup-submenu (cffi:null-pointer) menu))

(alias 'separator #'iup-cffi::%iup-separator)

(defattrfun label () (iup-cffi::%iup-label (cffi:null-pointer)))
(defattrfun config () (iup-cffi::%iup-config))

(alias 'config-load #'iup-cffi::%iup-config-load)
(alias 'config-save #'iup-cffi::%iup-config-save)

(alias 'config-dialog-show #'iup-cffi::%iup-config-dialog-show)
(alias 'config-dialog-closed #'iup-cffi::%iup-config-dialog-closed)

(defmacro with-iup (&body body)
  `(progn
     (iup:open)
     (unwind-protect
	 (progn
	   ,@body)
       (iup:close))))


;;; some controls have attrs that can be set at creation only e.g. IupDial
;;; some attrs are write only
;;; some attrs are read only
;;; some are not inheritable (maybe not applicable for Lisp binding?)
;;; attribute names have no spacing: ARROWIMAGEHIGHLIGHT -- convert to :arrow-image-highlight?

;;; containers are like controls but have children
;;; contrainers don't have callbacks??? nope -- some do: IupFrame
;;; some containers inherit from others (IupFlatFrame inheris from IupBackgroundBox

;;; callbacks are varied
