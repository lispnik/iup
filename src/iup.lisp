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

(defun attrs (handle)
  ;; FIXME implement
  (error "not implemented")
  )

(defun (setf attr-id) (new-value handle attr id)
  ;; FIXME implement
  )

(defun (setf attr-id-2) (new-value handle attr line column)
  ;; FIXME implement
  )

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

(defmacro with-iup (&body body)
  `(progn
     (iup:open)
     (unwind-protect
	 (progn
	   ,@body)
       (iup:close))))

(alias 'image-lib-open #'iup-cffi::%iup-image-lib-open)
(alias 'main-loop #'iup-cffi::%iup-main-loop)
(alias 'loop-step #'iup-cffi::%iup-loop-step)
(alias 'loop-step-wait #'iup-cffi::%iup-loop-step-wait)
(alias 'main-loop-level #'iup-cffi::%iup-main-loop-level)
(alias 'flush #'iup-cffi::%iup-flush)
(alias 'exit-loop #'iup-cffi::%iup-exit-loop)
(alias 'record-input #'iup-cffi::%iup-record-input)
(alias 'play-input #'iup-cffi::%iup-play-input)
(alias 'update #'iup-cffi::%iup-update)
(alias 'update-children #'iup-cffi::%iup-update-children)
(alias 'redraw #'iup-cffi::%iup-redraw)
(alias 'refresh #'iup-cffi::%iup-refresh)
(alias 'refresh-children #'iup-cffi::%iup-refresh-children)
(alias 'play-input #'iup-cffi::%iup-play-input)
(alias 'play-input #'iup-cffi::%iup-play-input)
(alias 'version #'iup-cffi::%iup-version)
(alias 'version-date #'iup-cffi::%iup-version-date)
(alias 'version-number #'iup-cffi::%iup-version-number)
(alias 'destroy #'iup-cffi::%iup-destroy)
(alias 'detach #'iup-cffi::%iup-detach)
(alias 'append #'iup-cffi::%iup-append)
(alias 'insert #'iup-cffi::%iup-insert)
(alias 'get-child #'iup-cffi::%iup-get-child)
(alias 'get-child-pos #'iup-cffi::%iup-get-child-pos)
(alias 'get-child-count #'iup-cffi::%iup-get-child-count)
(alias 'get-next-child #'iup-cffi::%iup-get-next-child)
(alias 'get-brother #'iup-cffi::%iup-get-brother)
(alias 'get-parent #'iup-cffi::%iup-get-parent)
(alias 'get-dialog #'iup-cffi::%iup-get-dialog)
(alias 'get-dialog-child #'iup-cffi::%iup-get-dialog-child)
(alias 'reparent #'iup-cffi::%iup-reparent)
(alias 'popup #'iup-cffi::%iup-popup)
(alias 'show #'iup-cffi::%iup-show)
(alias 'show-xy #'iup-cffi::%iup-show-xy)
(alias 'hide #'iup-cffi::%iup-hide)
(alias 'map #'iup-cffi::%iup-map)
(alias 'unmap #'iup-cffi::%iup-unmap)
(alias 'reset-attr #'iup-cffi::%iup-reset-attribute)
(alias 'set-gloabl #'iup-cffi::%iup-set-str-global)
(alias 'get-gloabl #'iup-cffi::%iup-get-global)
(alias 'set-focus #'iup-cffi::%iup-set-focus)
(alias 'get-focus #'iup-cffi::%iup-get-focus)
(alias 'get-previous-field #'iup-cffi::%iup-get-previous-field)
(alias 'get-next-field #'iup-cffi::%iup-get-next-field)

(defun get-all-classes-count ()
  (iup-cffi::%iup-get-all-classes (cffi:null-pointer) 0))

(defun get-all-classes ()
  (let* ((n (get-all-classes-count)))
    (with-foreign-object (array :string n)
      (iup-cffi::%iup-get-all-classes array n)
      (loop for i below n
	    collect (cffi:mem-aref array :string i)))))

(defun get-class-attributes-count (classname)
  (iup-cffi::%iup-get-class-attributes classname (cffi:null-pointer) 0))

(defun get-class-attributes (classname)
  (let* ((n (get-class-attributes-count classname)))
    (with-foreign-object (array :string n)
      (iup-cffi::%iup-get-class-attributes classname array n)
      ;; seems to be a sort of fixed array, null terminating mix??
      (loop for i below n
	    while (cffi:mem-aref array :string i)
	    collect (make-keyword (cffi:mem-aref array :string i))))))

;;; FIXME still memory faults
;; (with-iup (mapcar #'get-class-attributes (get-all-classes)))
;; (with-iup (get-all-classes))
;; (with-iup (get-class-attributes "canvas"))
;; (with-iup (loop for i in (get-all-classes) do (progn (print i) (print (get-class-attributes i)))))
;; (loop for i below 10 collect i)


(defattrfun fill () (iup-cffi::%iup-fill))
(defattrfun space () (iup-cffi::%iup-space))
(defattrfun radio (child) (iup-cffi::%iup-radio child))

(defmacro defattrfun-children (name func)
  `(defattrfun ,name (children)
     (let ((array (foreign-alloc 'iup-cffi::ihandle :initial-contents children :null-terminated-p t)))
       (unwind-protect
	    (,func array)
	 (foreign-free array)))))

(defattrfun-children vbox iup-cffi::%iup-vbox-v)
(defattrfun-children zbox iup-cffi::%iup-zbox-v)
(defattrfun-children hbox iup-cffi::%iup-hbox-v)
(defattrfun-children normalizer iup-cffi::%iup-normalizer-v)
(defattrfun-children cbox iup-cffi::%iup-cbox-v)

(defattrfun sbox (child) (iup-cffi::%iup-sbox child))
(defattrfun split (child1 child2) (iup-cffi::%iup-split child1 child2))
(defattrfun scroll-box (child) (iup-cffi::%iup-scroll-box child))
(defattrfun flat-scroll-box (child) (iup-cffi::%iup-flat-scroll-box child))

(defattrfun-children grid-box iup-cffi::%iup-gridbox-v)

(defattrfun expander (child) (iup-cffi::%iup-expander child))
(defattrfun detach-box (child) (iup-cffi::%iup-detach-box child))
(defattrfun background-box (child) (iup-cffi::%iup-background-box child))
(defattrfun frame (child) (iup-cffi::%iup-frame child))
(defattrfun flat-frame (child) (iup-cffi::%iup-flat-frame child))

;;; image...

(defattrfun item () (iup-cffi::%iup-item (cffi:null-pointer) (cffi:null-pointer)))

(alias 'separator #'iup-cffi::%iup-separator)

(defattrfun submenu (menu) (iup-cffi::%iup-submenu (cffi:null-pointer) menu))

(defattrfun-children menu iup-cffi::%iup-menu-v)

(defattrfun button () (iup-cffi::%iup-button (cffi:null-pointer) (cffi:null-pointer)))
(defattrfun flat-button () (iup-cffi::%iup-flat-button (cffi:null-pointer)))
(defattrfun flat-toggle () (iup-cffi::%iup-flat-toggle (cffi:null-pointer)))
(defattrfun drop-button () (iup-cffi::%iup-drop-button (cffi:null-pointer)))
(defattrfun flat-label () (iup-cffi::%iup-flat-label (cffi:null-pointer)))
(defattrfun flat-separator () (iup-cffi::%iup-flat-separator))
(defattrfun canvas () (iup-cffi::%iup-canvas (cffi:null-pointer)))
(defattrfun dialog (child) (iup-cffi::%iup-dialog child))
(defattrfun user () (iup-cffi::%iup-user))
(defattrfun label () (iup-cffi::%iup-label (cffi:null-pointer)))
(defattrfun list () (iup-cffi::%iup-list (cffi:null-pointer)))
(defattrfun text () (iup-cffi::%iup-text (cffi:null-pointer)))
(defattrfun multi-line () (iup-cffi::%iup-multi-line (cffi:null-pointer)))
(defattrfun toggle () (iup-cffi::%iup-toggle (cffi:null-pointer) (cffi:null-pointer)))
(defattrfun timer () (iup-cffi::%iup-timer))
(defattrfun clipboard () (iup-cffi::%iup-clipboard))
(defattrfun progress-bar () (iup-cffi::%iup-progress-bar))
(defattrfun val () (iup-cffi::%iup-val (cffi:null-pointer)))

(defattrfun-children tabs iup-cffi::%iup-tabs-v)
(defattrfun-children flat-tabs iup-cffi::%iup-flat-tabs-v)

(defattrfun tree () (iup-cffi::%iup-tree))
(defattrfun link () (iup-cffi::%iup-link (cffi:null-pointer) (cffi:null-pointer)))
(defattrfun animated-label () (iup-cffi::%iup-animated-label (cffi:null-pointer)))
(defattrfun date-pick () (iup-cffi::%iup-date-pick))
(defattrfun calendar () (iup-cffi::%iup-calendar))
(defattrfun colorbar () (iup-cffi::%iup-colorbar))
(defattrfun gauge () (iup-cffi::%iup-gauge))
(defattrfun dial () (iup-cffi::%iup-dial (cffi:null-pointer)))
(defattrfun color-browser () (iup-cffi::%iup-color-browser))
(defattrfun file-dialog () (iup-cffi::%iup-file-dlg))
(defattrfun message-dialog () (iup-cffi::%iup-message-dlg))
(defattrfun color-dialog () (iup-cffi::%iup-color-dlg))
(defattrfun font-dialog () (iup-cffi::%iup-font-dlg))
(defattrfun progress-dialog () (iup-cffi::%iup-progress-dlg))

(alias 'message #'iup-cffi::%iup-message)
(alias 'message-error #'iup-cffi::%iup-message-error)
(alias 'message-alarm #'iup-cffi::%iup-message-alarm)
(alias 'alarm #'iup-cffi::%iup-alarm)

(defattrfun config () (iup-cffi::%iup-config))

(alias 'config-load #'iup-cffi::%iup-config-load)
(alias 'config-save #'iup-cffi::%iup-config-save)
(alias 'config-dialog-show #'iup-cffi::%iup-config-dialog-show)
(alias 'config-dialog-closed #'iup-cffi::%iup-config-dialog-closed)
