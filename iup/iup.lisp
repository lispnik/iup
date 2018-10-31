(defpackage #:iup
  (:use #:common-lisp
	#:cffi
	#:alexandria)
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
	   #:version
	   #:version-date
	   #:version-number
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
	   #:attribute-handle
	   #:reset-attribute
	   #:set-global
	   #:get-global
	   #:set-focus
	   #:get-focus
	   #:previous-field
	   #:next-field

	   ;; TODO
	   ;; set-callback
	   ;; get-callback
	   ;; set-function
	   ;; get-function
	   ;; get-handle
	   ;; set-handle
	   ;; get-all-names
	   ;; get-all-dialogs
	   ;; get-name
	   
	   #:image
	   #:image-rgb
	   #:image-rgba

	   #:file-dialog
	   #:message-dialog
	   #:color-dialog
	   #:font-dialog
	   #:message
	   #:message-error
	   #:message-alarm
	   #:alarm
	   #:config
	   #:config-load
	   #:config-save
	   #:config-dialog-show
	   #:config-dialog-closed

	   #:all-classes
	   #:class-attributes
	   #:class-callbacks
	   #:class-name
	   #:class-type
	   #:save-class-attributes
	   #:copy-class-attributes
	   
	   #:with-iup
	   #:attribute
	   #:attribute-id
	   #:attribute-id-2
	   #:attributes
	   #:callback
	   #:handle
	   #:platform)
  (:shadow #:open
	   #:close
	   #:map
	   #:append
	   #:list
	   #:fill
	   #:space
	   #:class-name)
  (:import-from #:iup-utils
		#:alias))

(in-package #:iup)

(defmacro defiupclass (class package)
  (flet ((sort-attributes (attributes)
	   (sort (copy-seq attributes) #'string<
		 :key #'(lambda (attribute) (getf attribute :name))))
	 (has-flag-p (attribute flag)
	   (member flag (getf attribute :flags))))
    (let* ((all-attributes (sort-attributes (getf class :attributes)))
	   (attributes (remove-if #'(lambda (attribute)
				      (or (has-flag-p attribute :readonly)
					  (has-flag-p attribute :callback)
					  (has-flag-p attribute :has-id)
					  (has-flag-p attribute :has-id2)))
				  all-attributes))
	   (callbacks (remove-if-not #'(lambda (attribute)
					 (has-flag-p attribute :callback))
				     all-attributes))
	   (classname (getf class :classname))
	   (vanity-classname (getf class :vanity-classname))
	   (classname-symbol (intern (if vanity-classname vanity-classname (string-upcase classname))
				     (find-package package)))
	   (children-p (getf class :children-p))
	   (child-p (getf class :child-p)))
      (with-gensyms (handle)
	`(progn
	   (defun ,classname-symbol
	       (,@(cond (children-p '(children))
			(child-p '(child))
			(t nil))
		&rest attributes
		&key ,@(mapcar #'(lambda (attribute)
				   (let ((symbol (intern (getf attribute :name) (find-package package))))
				     (if (or (has-flag-p attribute :no-defaultvalue)
					     (not (getf attribute :default-value)))
					 symbol
					 (cl:list symbol (getf attribute :default-value)))))
			       attributes)
		  ,@(mapcar #'(lambda (attribute)
				(intern (getf attribute :name)))
			    callbacks))
	     (let ((,handle (iup-cffi::%iup-create ,classname)))
	       (loop for (attribute value) on attributes by #'cddr
;;;		     do (print (setf (attribute ,handle attribute) value))
		     do (iup-cffi::%iup-set-str-attribute ,handle attribute value
							  ))
	       (loop for c in ,(cond (children-p `children)
				     (child-p `(cl:list child))
				     (t nil))
		     do (print (iup:append ,handle c)))))
	   (export '(,classname-symbol) (find-package ,package)))))))

(defmacro defiupclasses (export-package)
  (let* ((classesdb (with-open-file (stream (asdf:system-relative-pathname "iup" "classesdb" :type "lisp-sexp"))
		      (let ((*read-eval* nil))
			(read stream))))
	 (platform-classes (getf (find (iup-utils:platform) classesdb
				       :key #'(lambda (platform) (getf platform :platform)))
				 :metadata))
	 (package (getf platform-classes :package))
	 (classes (getf platform-classes :classnames)))
    `(progn ,@(mapcar #'(lambda (class)
			  `(defiupclass ,class ,package))
		      classes))))

(defiupclasses "IUP")

#+null
(iup:with-iup ()
  (let* ((button (iup:button :title "Press Me"))
	 (hbox (iup:vbox (list button)))
	 (dialog (iup:dialog hbox)))
    (iup:show dialog)
    (iup:main-loop)))


(defun handle-p (handle)
  (and (cffi:pointerp handle)
       (not (cffi:null-pointer-p handle))
       (= (char-code #\I) (cffi:mem-ref handle :char 0))
       (= (char-code #\U) (cffi:mem-ref handle :char 1))
       (= (char-code #\P) (cffi:mem-ref handle :char 2))
       (zerop (cffi:mem-ref handle :char 3))))

(defun get-classname-names (classname name-producer)
  (let ((max-n (funcall name-producer classname (cffi:null-pointer) 0)))
    (unless (= max-n -1)
      (let ((array (cffi:foreign-alloc :pointer :initial-element (cffi:null-pointer) :count max-n :null-terminated-p t)))
	(unwind-protect
	     (progn
	       (funcall name-producer classname array max-n)
	       (loop for i below max-n
		     for ref = (cffi:mem-aref array :pointer i)
		     until (cffi:null-pointer-p ref)
		     collect (make-keyword (cffi:foreign-string-to-lisp ref))))
	  (foreign-free array))))))

(defun class-attributes (classname)
  (get-classname-names classname #'iup-cffi::%iup-get-class-attributes))

(defun class-callbacks (classname)
  (get-classname-names classname #'iup-cffi::%iup-get-class-callbacks))

(defun all-classes ()
  (let* ((max-n (iup-cffi::%iup-get-all-classes (cffi:null-pointer) 0))
         (array (cffi:foreign-alloc :pointer :initial-element (cffi:null-pointer) :count max-n :null-terminated-p t)))
    (unwind-protect
         (progn
           (iup-cffi::%iup-get-all-classes array max-n)
           (loop for i below max-n
                 for ref = (cffi:mem-aref array :pointer i)
                 until (cffi:null-pointer-p ref)
                 collect (cffi:foreign-string-to-lisp ref) into result
		 finally (return (sort result #'string<))))
      (cffi:foreign-free array))))

(defun class-name (handle)
  (iup-cffi::%iup-get-class-name handle))

(defun class-type (handle)
  (iup-cffi::%iup-get-class-type handle))

(alias 'save-class-attributes       #'iup-cffi::%iup-save-class-attributes)
(alias 'copy-class-attributes       #'iup-cffi::%iup-copy-class-attributes)
(alias 'set-class-default-attribute #'iup-cffi::%iup-set-class-default-attribute)

(defun attribute-handle (handle name)
  (iup-cffi::%iup-get-attribute-handle handle name))

(defun (setf attribute-handle) (new-value handle name)
  (iup-cffi::%iup-set-attribute-handle handle name new-value)
  new-value)

(defun callback (handle name)
  (iup-cffi::%iup-get-callback handle name))

(defun (setf callback) (new-value handle name)
  (iup-cffi::%iup-set-callback
   handle
   name
   (if new-value
       (cffi:get-callback new-value)
       (cffi:null-pointer)))
  new-value)

(deftype attribute-type () '(member :int :float :double :string :pointer))

(defun attribute (handle attribute &optional (type :string))
  (declare (type attribute-type type))
  (case type
    (:int (iup-cffi::%iup-get-int-attribute handle attribute))
    (:float (iup-cffi::%iup-get-float-attribute handle attribute))
    (:double (iup-cffi::%iup-get-double-attribute handle attribute))
    (:string (iup-cffi::%iup-get-attribute handle attribute))
    (:pointer (iup-cffi::%iup-get-pointer-attribute handle attribute))))

(defun (setf attribute) (new-value handle attribute)
  (typecase new-value
    (string (iup-cffi::%iup-set-str-attribute handle attribute (or new-value (cffi:null-pointer))))
    (integer (iup-cffi::%iup-set-int-attribute handle attribute new-value))
    (single-float (iup-cffi::%iup-set-float-attribute handle attribute new-value))
    (double-float (iup-cffi::%iup-set-double-attribute handle attribute new-value))
    (t (iup-cffi::%iup-set-str-attribute
	handle
	attribute
	(if new-value
	    (princ new-value)
	    (cffi:null-pointer)))))
  new-value)

(defun (setf attributes) (attributes handle)
  (loop for (attribute value) on attributes by #'cddr
        do (progn
             (setf (attribute handle attribute) value))
        finally (return attributes)))

(defun attributes (handle)
;;   (iup-cffi::%iup-get-all-attributes)
  ;; FIXME implement
  (error "not implemented")
  )

(defun (setf attribute-id) (new-value handle attribute id)
  ;; FIXME implement
  )

(defun attribute-id-2 (handle attribute line column &optional (type :string))
  (declare (type attribute-type type))
  (case type
    (:int (iup-cffi::%iup-get-int-attribute-id-2 handle attribute line column))
    (:float (iup-cffi::%iup-get-float-attribute-id-2 handle attribute line column))
    (:double (iup-cffi::%iup-get-double-attribute-id-2 handle attribute line column))
    (:string (iup-cffi::%iup-get-attribute-id-2 handle attribute line column))))
  
(defun (setf attribute-id-2) (new-value handle attribute line column)
  (typecase new-value
    (string (iup-cffi::%iup-set-str-attribute-id-2 handle attribute line column (or new-value (cffi:null-pointer))))
    (integer (iup-cffi::%iup-set-int-attribute-id-2 handle attribute line column new-value))
    (single-float (iup-cffi::%iup-set-float-attribute-id-2 handle attribute line column new-value))
    (double-float (iup-cffi::%iup-set-double-attribute-id-2 handle attribute line column new-value))
    (t (iup-cffi::%iup-set-str-attribute-id-2
	handle
	attribute
	line
	column
	(if new-value
	    (princ new-value)
	    (cffi:null-pointer))))))

(defun (setf attribute-callback-handle-dwim) (new-value handle name)
  (let ((name-string (symbol-name name)))
    (cond
      ((or (string= "ACTION" name-string)
	   (ends-with-subseq "_CB" name-string)
           (starts-with-subseq "K_" name-string))
       (setf (callback handle name) new-value))
      ((handle-p new-value)
       (setf (attribute-handle handle name) new-value))
      (t
       (setf (attribute handle name) new-value)))))

(defun (setf attribute-callback-handles-dwim) (names handle)
  (loop for (name value) on names by #'cddr
        do (progn
             (setf (attribute-callback-handle-dwim handle name) value))
        finally (return handle)))

(defun (setf handle) (new-value name)
  (iup-cffi::%iup-set-handle name new-value))

(defun handle (name)
  (iup-cffi::%iup-get-handle name))

(defmacro defattributefun (name args &rest body)
  `(defun ,name (,@args &rest attributes &key &allow-other-keys)
     (setf (attribute-callback-handles-dwim (progn ,@body)) attributes)))

;; (defmacro defattributefun (name args &rest body)
;;   `(defun ,name (,@args &rest attributes &key &allow-other-keys)
;;      (setf (attributes (progn ,@body)) attributes)))

(defconstant +error+      1)
(defconstant +noerror+    0)
(defconstant +opened+     -1)
(defconstant +invalid+    -1)
(defconstant +invalid-id+ -10)

(defconstant +ignore+   -1)
(defconstant +default+  -2)
(defconstant +close+    -3)
(defconstant +continue+ -4)

(defconstant +center+       #xffff)
(defconstant +left+         #xfffe)
(defconstant +right+        #xffd)
(defconstant +mousepos+     #xfffc)
(defconstant +current+      #xfffb)
(defconstant +centerparent+ #xfffa)
(defconstant +top+          +left+)
(defconstant +bottom+       +right+)

(defconstant +primary+   -1)
(defconstant +secondary+ -2)

(defconstant +recbinary+ 0)
(defconstant +rectext+   1)

(defun open ()
  (let ((ret (iup-cffi::%iup-open (cffi:null-pointer) (cffi:null-pointer))))
    (when (= ret +error+)
      (error "Can't initialize IUP"))))

(alias 'close #'iup-cffi::%iup-close)

(defmacro with-iup (() &body body)
  `(progn
     (iup:open)
     (unwind-protect
         (progn
           ,@body)
       (iup:close))))

(alias 'image-lib-open          #'iup-cffi::%iup-image-lib-open)
(alias 'main-loop               #'iup-cffi::%iup-main-loop)
(alias 'loop-step               #'iup-cffi::%iup-loop-step)
(alias 'loop-step-wait          #'iup-cffi::%iup-loop-step-wait)
(alias 'main-loop-level         #'iup-cffi::%iup-main-loop-level)
(alias 'flush                   #'iup-cffi::%iup-flush)
(alias 'exit-loop               #'iup-cffi::%iup-exit-loop)
(alias 'record-input            #'iup-cffi::%iup-record-input)
(alias 'play-input              #'iup-cffi::%iup-play-input)
(alias 'update                  #'iup-cffi::%iup-update)
(alias 'update-children         #'iup-cffi::%iup-update-children)
(alias 'redraw                  #'iup-cffi::%iup-redraw)
(alias 'refresh                 #'iup-cffi::%iup-refresh)
(alias 'refresh-children        #'iup-cffi::%iup-refresh-children)
(alias 'play-input              #'iup-cffi::%iup-play-input)
(alias 'version                 #'iup-cffi::%iup-version)
(alias 'version-date            #'iup-cffi::%iup-version-date)
(alias 'version-number          #'iup-cffi::%iup-version-number)
(alias 'destroy                 #'iup-cffi::%iup-destroy)
(alias 'detach                  #'iup-cffi::%iup-detach)
(alias 'append                  #'iup-cffi::%iup-append)
(alias 'insert                  #'iup-cffi::%iup-insert)
(alias 'get-child               #'iup-cffi::%iup-get-child)
(alias 'get-child-pos           #'iup-cffi::%iup-get-child-pos)
(alias 'get-child-count         #'iup-cffi::%iup-get-child-count)
(alias 'get-next-child          #'iup-cffi::%iup-get-next-child)
(alias 'get-brother             #'iup-cffi::%iup-get-brother)
(alias 'get-parent              #'iup-cffi::%iup-get-parent)
(alias 'get-dialog              #'iup-cffi::%iup-get-dialog)
(alias 'get-dialog-child        #'iup-cffi::%iup-get-dialog-child)
(alias 'reparent                #'iup-cffi::%iup-reparent)
(alias 'popup                   #'iup-cffi::%iup-popup)
(alias 'show                    #'iup-cffi::%iup-show)
(alias 'show-xy                 #'iup-cffi::%iup-show-xy)
(alias 'hide                    #'iup-cffi::%iup-hide)
(alias 'map                     #'iup-cffi::%iup-map)
(alias 'unmap                   #'iup-cffi::%iup-unmap)
(alias 'reset-attribute         #'iup-cffi::%iup-reset-attribute)
(alias 'set-global              #'iup-cffi::%iup-set-str-global)
(alias 'get-global              #'iup-cffi::%iup-get-global)
(alias 'set-focus               #'iup-cffi::%iup-set-focus)
(alias 'get-focus               #'iup-cffi::%iup-get-focus)
(alias 'previous-field          #'iup-cffi::%iup-previous-field)
(alias 'next-field              #'iup-cffi::%iup-next-field)



;;;  IMAGES?

(defattributefun image (width height pixels)
  (let ((array (cffi:foreign-alloc :unsigned-char :initial-contents pixels :count (* width height))))
    (unwind-protect
	 (iup-cffi::%iup-image width height array)
      (cffi:foreign-free array))))

(defattributefun image-rgb (width height pixels)
  (let ((array (cffi:foreign-alloc :unsigned-char :initial-contents pixels :count (* width height 3))))
    (unwind-protect
	 (iup-cffi::%iup-image-rgb width height array)
      (cffi:foreign-free array))))

(defattributefun image-rgba (width height pixels)
  (let ((array (cffi:foreign-alloc :unsigned-char :initial-contents pixels :count (* width height 4))))
    (unwind-protect
	 (iup-cffi::%iup-image-rgba width height array)
      (cffi:foreign-free array))))

(alias 'message       #'iup-cffi::%iup-message)
(alias 'message-error #'iup-cffi::%iup-message-error)
(alias 'message-alarm #'iup-cffi::%iup-message-alarm)
(alias 'alarm         #'iup-cffi::%iup-alarm)

(defattributefun config () (iup-cffi::%iup-config))

(alias 'config-load          #'iup-cffi::%iup-config-load)
(alias 'config-save          #'iup-cffi::%iup-config-save)
(alias 'config-dialog-show   #'iup-cffi::%iup-config-dialog-show)
(alias 'config-dialog-closed #'iup-cffi::%iup-config-dialog-closed)
