(in-package #:iup)

(defparameter *iup-callback-encoding*
  '((#\c . :unsigned-char)
    (#\I . :pointer)    ;*int
    (#\i . :int)
    (#\f . :float)
    (#\d . :double)
    (#\s . :string)
    (#\V . :pointer)    ;*void
    (#\C . :pointer)    ;*cdCanvas
    (#\v . :pointer)    ;FIXME Asked about this on mailing list, confirmed it should be "C"
    (#\n . tecgraf-base:ihandle)))

(defun class-callback-name (classname callback-name package)
  (declare (ignore package))
  (make-keyword (format nil "~:@(~A-~A~)" classname callback-name)))

(defun check-callback-args (action args-list)
  (declare (ignore action args-list))
  ;; FIXME should have enough info to check callback args at runtime
  ;; based on the compile time metadata
  t)

(defun invoke-callback (action action-args-list args-list-names)
  (if (check-callback-args action args-list-names)
      (let ((result (restart-case
			(apply action action-args-list)
		      (:use-default ()
		       :report "Continue by returning IUP:+DEFAULT+"
			iup:+default+))))
	(if (null result)
	    (restart-case
		(error "Callback returned NIL")
	      (:use-default ()
	       :report "Continue by returning IUP:+DEFAULT+"
		iup:+default+))
	    result))
      (restart-case 
	  (error "Callback arguments list does not conform to to expected arguments list")
	(:continue ()
	 :report "Continue without invoking the callback")
	(:danger-zone ()
	 :report "Call it anyway"
	  (apply action action-args-list)))))

(defmacro defclasscallback (classname name spec package)
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
                         finally (return (list* '(arg0 tecgraf-base:ihandle) arg-list))))
         (return-and-arg-list (cl:list return-type arg-list))
         (callback-name (class-callback-name classname name package)))
    `(cffi:defcallback ,callback-name ,@return-and-arg-list
	 (let* ((action (find-callback ',callback-name arg0))
		(args-list-names (cl:list ,@(mapcar #'car arg-list)))
		(action-args-list (cl:list ,@(mapcar #'car arg-list))))
	   (invoke-callback action action-args-list args-list-names)))))

(defun %iup-image (width height pixels)
  (assert (= (length pixels) (* width height)))
  (let ((ptr (cffi:foreign-alloc :unsigned-char :initial-contents pixels)))
    (unwind-protect
	 (iup-cffi::%iup-image width height ptr)
      (cffi:foreign-free ptr))))

(defun %iup-image-rgb (width height pixels)
  (assert (= (length pixels) (* 3 width height)))
  (let ((ptr (cffi:foreign-alloc :unsigned-char :initial-contents pixels)))
    (unwind-protect
	 (iup-cffi::%iup-image-rgb width height ptr)
      (cffi:foreign-free ptr))))

(defun %iup-image-rgba (width height pixels)
  (assert (= (length pixels) (* 4 width height)))
  (let ((ptr (cffi:foreign-alloc :unsigned-char :initial-contents pixels)))
    (unwind-protect
	 (iup-cffi::%iup-image-rgba width height ptr)
      (cffi:foreign-free ptr))))

(defparameter *iup-image-constructor-alist*
  '(("image" . %iup-image)
    ("imagergb" . %iup-image-rgb)
    ("imagergba" . %iup-image-rgba)))

(defun image-class (classname)
  (assoc-value *iup-image-constructor-alist* classname :test #'string=))

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
           (children (getf class :children))
	   (image-class (image-class classname))
	   (fixed-args (if image-class '(width height pixels) nil))
	   (constructor-args (if image-class '(width height pixels) (cl:list classname)))
	   (class-constructor (or (image-class classname) 'iup-cffi::%iup-create))
           (key-args (mapcar #'(lambda (attribute)
                                 (let ((symbol (intern (getf attribute :name) (find-package package))))
                                   (if (or (has-flag-p attribute :iupaf-no-defaultvalue)
                                           (not (getf attribute :default-value)))
                                       symbol
                                       (cl:list symbol (getf attribute :default-value)))))
                             attributes))
           (key-args-names (mapcar #'(lambda (key-arg)
                                       (if (consp key-arg) (car key-arg) key-arg))
                                   key-args)))
      (with-gensyms (handle)
        `(progn
           (defun ,classname-symbol
               (,@fixed-args
		,@(case children
                    (:child-many '(children))
                    (:child-none nil)
                    (otherwise (if (= 1 children)
                                   '(child)
                                   (loop for i from 0 below children
                                         collect (intern (format nil "CHILD~A" (1+ i)))))))
                &rest attributes
                &key ,@key-args ,@callback-names)
             (declare (ignore ,@key-args-names ,@callback-names))
             (let ((,handle (,class-constructor ,@constructor-args)))
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
                                (class-callback-name ,classname attribute (find-package ,package))))
                              (register-callback (class-callback-name ,classname attribute (find-package ,package))
                                                 ,handle
                                                 value))
                            (iup-cffi::%iup-set-str-attribute ,handle attribute (princ-to-string value))))
               (loop for c in ,(case children
                                 (:child-many 'children)
                                 (:child-none nil)
                                 (otherwise (if (= 1 children)
                                                '(cl:list child)
                                                (cons 'cl:list
                                                      (loop for i from 0 below children
                                                            collect (intern (format nil "CHILD~A" (1+ i))))))))
                     do (iup-cffi::%iup-append ,handle c))
               (when-let ((handlename (getf attributes :handlename)))
                 (setf (iup:handle handlename) ,handle))
               ,handle))
           (export '(,classname-symbol) (find-package ,package))
           ,@(let ((callback-attributes (remove-if-not #'(lambda (attribute)
                                                           (has-flag-p attribute :iupaf-callback))
                                                       all-attributes)))
               (loop for attribute in callback-attributes
                     for spec = (getf attribute :default-value)
                     for name = (make-keyword (getf attribute :name))
                     collect
                     `(defclasscallback ,classname ,name ,spec ,package))))))))

(defmacro defiupclasses (export-package)
  (let* ((classesdb (with-open-file
                        (stream (asdf:system-relative-pathname "iup" "classesdb" :type "lisp-sexp"))
                      (let ((*read-eval* nil))
                        (read stream))))
         (platform-classes (getf (find (iup-utils:platform) classesdb
                                       :key #'(lambda (platform) (getf platform :platform)))
                                 :metadata))
         (package (find export-package
                        platform-classes
                        :key #'(lambda (package)
                                 (getf package :package))
                        :test #'string=))
         (classes (getf package :classnames)))
    `(progn ,@(mapcar #'(lambda (class)
                          `(defiupclass ,class ,(getf package :package)))
                      classes))))
