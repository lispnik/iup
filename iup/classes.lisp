(in-package #:iup)

(defparameter *iup-callback-encoding*
  '((#\c . :unsigned-char)
    (#\I . :pointer)	;*int
    (#\i . :int)
    (#\f . :float)
    (#\d . :double)
    (#\s . :string)
    (#\V . :pointer)    ;*void
    (#\C . :pointer)	;*cdCanvas
    (#\n . :pointer))) 	;*Ihandle

(defun class-callback-name (classname callback-name)
  (format nil "~:@(~A-~A~)" classname callback-name))

(defun check-callback-args (action args-list)
  (declare (ignore action args-list))
  ;; FIXME
  t)

(defmacro defclasscallback (classname name spec)
  (let* ((return-type (or (and (find #\= spec)
			       (assoc-value *iup-callback-encoding*
					    (elt spec (1- (length spec)))
					    :test #'char=))
			  :int))
	 (arg-list (loop for i from 1
			 for c across spec
			 until (char= c #\=)
			 for s = (assoc-value *iup-callback-encoding* c :test #'char=)
			 for arg = (intern (format nil "ARG~A" i))
			 collect (cl:list arg s) into arg-list
			 finally (return (list* '(arg0 :pointer) arg-list))))
	 (return-and-arg-list (cl:list return-type arg-list))
	 (callback-name (intern (class-callback-name classname name))))
    `(cffi:defcallback ,callback-name ,@return-and-arg-list
	 (let ((action (genhash:hashref (make-callback :name ',callback-name :handle arg0)
					*registered-callbacks*))
	       (args-list-names (cl:list ,@(mapcar #'car arg-list))))
	   (if (check-callback-args action args-list-names)
	       (funcall action ,@(mapcar #'car arg-list))
	       (restart-case 
		   (error "Callback arguments list does not conform to to expected arguments list")
		 (:continue ()
		  :report "Continue without invoking the callback")
		 (:danger-zone ()
		  :report "Call it anyway"
		   (funcall action args-list))))))))

(defmacro defiupclass (class package)
  (flet ((sort-attributes (attributes)
	   (sort (copy-seq attributes) #'string<
		 :key #'(lambda (attribute) (getf attribute :name))))
	 (has-flag-p (attribute flag)
	   (member flag (getf attribute :flags))))
    (let* ((all-attributes (sort-attributes (getf class :attributes)))
	   (attributes (remove-if #'(lambda (attribute)
				      (or (has-flag-p attribute :iupaf-readonly)
					  (has-flag-p attribute :iupaf-callback)
					  (has-flag-p attribute :iupaf-has-id)
					  (has-flag-p attribute :iupaf-has-id2)))
				  all-attributes))
	   (callbacks (remove-if-not #'(lambda (attribute)
					 (has-flag-p attribute :iupaf-callback))
				     all-attributes))
	   (callback-names (mapcar #'(lambda (attribute)
				       (intern (getf attribute :name)))
				   callbacks))
	   (classname (getf class :classname))
	   (vanity-classname (getf class :vanity-classname))
	   (classname-symbol (intern (if vanity-classname vanity-classname (string-upcase classname))
				     (find-package package)))
	   (children (getf class :children)))
      (with-gensyms (handle)
	`(progn
	   (defun ,classname-symbol
	       (,@(case children
		    (:child-many '(children))
		    (:child-none nil)
		    (otherwise (if (= 1 children)
				   '(child)
				   (loop for i from 0 below children
					 collect (intern (format nil "CHILD~A" (1+ i)))))))
		&rest attributes
		&key ,@(mapcar #'(lambda (attribute)
				   (let ((symbol (intern (getf attribute :name) (find-package package))))
				     (if (or (has-flag-p attribute :iupaf-no-defaultvalue)
					     (not (getf attribute :default-value)))
					 symbol
					 (cl:list symbol (getf attribute :default-value)))))
			       attributes)
		  ,@callback-names)
	     (let ((,handle (iup-cffi::%iup-create ,classname)))
	       (loop for (attribute value) on attributes by #'cddr
		     do (if (member attribute ',(mapcar #'(lambda (attribute)
							    (make-keyword (getf attribute :name)))
							callbacks))
			    (progn
			      (iup-cffi::%iup-set-callback
			       ,handle
			       (symbol-name attribute)
			       ;; FIXME extract repeated form in the following two
			       (cffi:get-callback
				(intern (class-callback-name ,classname attribute)
					(find-package ,package))))
			      (register-callback (intern (class-callback-name ,classname attribute)
							 (find-package ,package))
						 ,handle
						 value))
			    (iup-cffi::%iup-set-str-attribute ,handle attribute (princ-to-string value))))
	       (loop for c in ,(case children
				 (:child-many 'children)
				 (:child-none nil)
				 (otherwise '(cl:list child)))
		     do (iup-cffi::%iup-append ,handle c))
	       ,handle))
	   (export '(,classname-symbol) (find-package ,package))
	   ,@(let ((callback-attributes (remove-if-not #'(lambda (attribute)
							   (has-flag-p attribute :iupaf-callback))
						       all-attributes)))
	       (loop for attribute in callback-attributes
		     for spec = (getf attribute :default-value)
		     for name = (make-keyword (getf attribute :name))
		     collect
		     `(defclasscallback ,classname ,name ,spec))))))))


(defmacro defiupclasses (export-package)
  (let* ((classesdb (with-open-file
			(stream (asdf:system-relative-pathname "iup" "classesdb" :type "lisp-sexp"))
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
