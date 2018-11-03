(in-package #:iup)

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
		  ,@(mapcar #'(lambda (attribute)
				(intern (getf attribute :name)))
			    callbacks))
	     (let ((,handle (iup-cffi::%iup-create ,classname)))
	       (loop for (attribute value) on attributes by #'cddr
		     do (iup-cffi::%iup-set-str-attribute ,handle attribute value))
	       (loop for c in ,(case children
				 (:child-many 'children)
				 (:child-none nil)
				 (otherwise '(cl:list child)))
		     do (iup-cffi::%iup-append ,handle c))
	       ,handle))
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

(defparameter *spec-code-cffi-type*
  '((#\c . :unsigned-char)
    (#\I . :pointer)	;*int
    (#\i . :int)
    (#\f . :float)
    (#\d . :double)
    (#\s . :string)
    (#\V . :pointer)    ;*void
    (#\C . :pointer)	;*cdCanvas
    (#\n . :pointer))) 	;*Ihandle

(defun create-callback (name spec)
  (let* ((return-type (or (and (find #\= spec)
			       (assoc-value *spec-code-cffi-type*
					    (elt spec (1- (length spec)))
					    :test #'char=))
			  :int))
	 (return-and-arg-list 
	   (loop for i from 1
		 for c across spec
		 until (char= c #\=)
		 for s = (assoc-value *spec-code-cffi-type* c :test #'char=)
		 for arg = (intern (format nil "ARG~A" i))
		 collect (cl:list arg s) into arg-list
		 finally (return (cl:list return-type (list* '(arg0 :pointer) arg-list))))))
    `(cffi:defcallback ,@return-and-arg-list
	 (push (make-event :name ,name :handle arg0) *event-list*)
	 iup::+ignore+)))

(let ((name :dragbegin_cb)
      (spec "ii=s"))
  (create-callback name spec))

;; /** Register the parameters of a callback. \n
;; * Format follows the \ref iupcbs.h header definitions. \n
;; * Notice that these definitions are similar to the class registration
;; * but have several differences and conflicts, for backward compatibility reasons. \n
;; * It can have none, one or more of the following. \n
;; * - "c" = (unsigned char) - byte
;; * - "i" = (int) - integer
;; * - "I" = (int*) - array of integers or pointer to integer
;; * - "f" = (float) - real
;; * - "d" = (double) - real
;; * - "s" = (char*) - string 
;; * - "V" = (void*) - generic pointer 
;; * - "C" = (struct _cdCanvas*) - cdCanvas* structure, used along with the CD library
;; * - "n" = (Ihandle*) - element handle
;; * The default return value for all callbacks is "i" (int), 
;; * but a different return value can be specified using one of the above parameters, 
;; * after all parameters using "=" to separate it from them.
;; * \ingroup iclass */
;; void iupClassRegisterCallback(Iclass* ic, const char* name, const char* format);
