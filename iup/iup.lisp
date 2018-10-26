(in-package #:iup)

(defun platform ()
  "Mapping from trivial-features -provided features to classesdb platform keywords."
  #+windows :windows
  #+linux :linux
  #+(and unix (not linux)) :unix)


;; (defmacro defiupclasses ()
;;   (let* ((classesdb (with-open-file (stream (asdf:system-relative-pathname "iup" "classesdb" :type "lisp-sexp"))
;; 		      (let ((*read-eval* nil))
;; 			(read stream))))
;; 	 (platform-classes (getf (find (platform) classesdb :key #'(lambda (platform) (getf platform :platform))) :metadata))
;; 	 (package (getf platform-classes :package))
;; 	 (classes (getf platform-classes :classnames)))
;;     `(progn
;;        ,@(mapcar #'(lambda (class) `(defiupclass ,class ,package)) ,classes))))


(defmacro defiupclass (class package)
  (flet ((sort-attributes (attributes)
	   (sort (copy-seq attributes) #'string<
		 :key #'(lambda (attribute) (getf attribute :name))))
	 (has-flag-p (attribute flag)
	   (member flag (getf attribute :flags))))
    (let* ((all-attributes (sort-attributes (getf class :attributes)))
	   (attributes (remove-if #'(lambda (attribute)
				      (or (has-flag-p attribute :readonly)
					  (has-flag-p attribute :callback)))
				  all-attributes))
	   (callbacks (remove-if-not #'(lambda (attribute)
					 (has-flag-p attribute :callback))
				     all-attributes))
	   (classname (getf class :classname))
	   (classname-symbol (intern (getf class :vanity-classname) (find-package package)))
	   (children-p (getf class :children-p))
	   (child-p (getf class :children-p)))
      (with-gensyms (handle)
	`(progn
	   (defun ,classname-symbol
	       (,@(cond (children-p 'children)
			(child-p 'child)
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
		     do (setf (attribute ,handle attribute) value))))
	   (export '(,classname-symbol) (find-package ,package)))))))

(defiupclass
    (:CLASSNAME "animatedlabel" :CHILD-P NIL :CHILDREN-P NIL :OVERRIDE-P NIL :VANITY-CLASSNAME
		"ANIMATED-LABEL" :ATTRIBUTES
		((:NAME "FGCOLOR" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P T :DEFAULT-VALUE "DLGFGCOLOR"
		  :SYSTEM-DEFAULT "DLGFGCOLOR" :FLAGS NIL)
		 (:NAME "HANDLENAME" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE NIL
		  :SYSTEM-DEFAULT NIL :FLAGS (:NO-INHERIT :NOT-MAPPED))
		 (:NAME "TIPBGCOLOR" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE "255 255 225"
		  :SYSTEM-DEFAULT "255 255 225" :FLAGS (:NOT-MAPPED))
		 (:NAME "WORDWRAP" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT
			NIL :FLAGS NIL)
		 (:NAME "FRAMETIME" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P T :DEFAULT-VALUE "30" :SYSTEM-DEFAULT
			"30" :FLAGS (:NO-INHERIT :NOT-MAPPED))
		 (:NAME "TIPICON" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE NIL :SYSTEM-DEFAULT
			NIL :FLAGS (:NOT-MAPPED))
		 (:NAME "MAXSIZE" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P T :DEFAULT-VALUE "65535x65535"
		  :SYSTEM-DEFAULT "65535x65535" :FLAGS (:NO-INHERIT :NOT-MAPPED))
		 (:NAME "SCREENPOSITION" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P NIL :DEFAULT-VALUE NIL
		  :SYSTEM-DEFAULT NIL :FLAGS (:NO-INHERIT :READONLY))
		 (:NAME "POSITION" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT NIL
		  :FLAGS (:NO-INHERIT :NO-DEFAULTVALUE :NOT-MAPPED :NO-SAVE))
		 (:NAME "FRAMECOUNT" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P NIL :DEFAULT-VALUE NIL :SYSTEM-DEFAULT
			NIL :FLAGS (:NO-INHERIT :NOT-MAPPED :READONLY))
		 (:NAME "DROPMOTION_CB" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE "iis"
		  :SYSTEM-DEFAULT NIL :FLAGS (:NO-INHERIT :CALLBACK))
		 (:NAME "IMINACTIVE" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT
			NIL :FLAGS (:NO-INHERIT :NO-DEFAULTVALUE :IHANDLENAME))
		 (:NAME "RUNNING" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P NIL :DEFAULT-VALUE NIL :SYSTEM-DEFAULT NIL
		  :FLAGS (:NO-INHERIT :NOT-MAPPED :READONLY))
		 (:NAME "DROPFILESTARGET" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P T :DEFAULT-VALUE NIL
		  :SYSTEM-DEFAULT NIL :FLAGS (:NO-INHERIT))
		 (:NAME "TIP" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT NIL
		  :FLAGS (:NO-INHERIT :NO-DEFAULTVALUE))
		 (:NAME "DRAGEND_CB" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE "i"
		  :SYSTEM-DEFAULT NIL :FLAGS (:NO-INHERIT :CALLBACK))
		 (:NAME "CANFOCUS" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE "NO" :SYSTEM-DEFAULT
			"NO" :FLAGS (:NO-INHERIT))
		 (:NAME "DRAGSOURCEMOVE" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE NIL
		  :SYSTEM-DEFAULT NIL :FLAGS (:NO-INHERIT))
		 (:NAME "VISIBLE" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P T :DEFAULT-VALUE "YES" :SYSTEM-DEFAULT
			"NO" :FLAGS (:NO-SAVE))
		 (:NAME "SEPARATOR" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT NIL
		  :FLAGS (:NO-INHERIT :NOT-MAPPED))
		 (:NAME "IMAGE" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT NIL
		  :FLAGS (:NO-INHERIT :NO-DEFAULTVALUE :IHANDLENAME))
		 (:NAME "DRAGBEGIN_CB" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE "ii"
		  :SYSTEM-DEFAULT NIL :FLAGS (:NO-INHERIT :CALLBACK))
		 (:NAME "ZORDER" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT NIL
		  :FLAGS (:NO-INHERIT :WRITEONLY))
		 (:NAME "X" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P NIL :DEFAULT-VALUE NIL :SYSTEM-DEFAULT NIL
		  :FLAGS (:NO-INHERIT :READONLY))
		 (:NAME "Y" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P NIL :DEFAULT-VALUE NIL :SYSTEM-DEFAULT NIL
		  :FLAGS (:NO-INHERIT :READONLY))
		 (:NAME "DRAGDROP" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT
			NIL :FLAGS (:NO-INHERIT))
		 (:NAME "MOTION_CB" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE "iis"
		  :SYSTEM-DEFAULT NIL :FLAGS (:NO-INHERIT :CALLBACK))
		 (:NAME "EXPAND" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT NIL
		  :FLAGS (:NO-INHERIT :NOT-MAPPED))
		 (:NAME "SIZE" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT NIL
		  :FLAGS (:NO-INHERIT :NO-DEFAULTVALUE :NOT-MAPPED :NO-SAVE))
		 (:NAME "PADDING" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P T :DEFAULT-VALUE "0x0" :SYSTEM-DEFAULT
			"0x0" :FLAGS (:NOT-MAPPED))
		 (:NAME "WID" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P NIL :DEFAULT-VALUE NIL :SYSTEM-DEFAULT NIL
		  :FLAGS (:NO-INHERIT :NO-STRING :READONLY))
		 (:NAME "TIPMARKUP" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE NIL :SYSTEM-DEFAULT
			NIL :FLAGS (:NOT-MAPPED))
		 (:NAME "FONTSIZE" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT NIL
		  :FLAGS (:NO-INHERIT :NOT-MAPPED :NO-SAVE))
		 (:NAME "NATURALSIZE" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P NIL :DEFAULT-VALUE NIL :SYSTEM-DEFAULT
			NIL :FLAGS (:NO-INHERIT :NO-DEFAULTVALUE :NOT-MAPPED :READONLY))
		 (:NAME "DROPTYPES" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT
			NIL :FLAGS (:NO-INHERIT :NOT-MAPPED))
		 (:NAME "USERSIZE" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT NIL
		  :FLAGS (:NO-INHERIT :NO-DEFAULTVALUE :NOT-MAPPED))
		 (:NAME "TIPDELAY" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE "5000"
		  :SYSTEM-DEFAULT "5000" :FLAGS (:NOT-MAPPED))
		 (:NAME "MAP_CB" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE "" :SYSTEM-DEFAULT NIL
		  :FLAGS (:NO-INHERIT :CALLBACK))
		 (:NAME "TITLE" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT NIL
		  :FLAGS (:NO-INHERIT :NO-DEFAULTVALUE))
		 (:NAME "PROPAGATEFOCUS" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE "NO"
		  :SYSTEM-DEFAULT "NO" :FLAGS (:NO-INHERIT))
		 (:NAME "BGCOLOR" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P T :DEFAULT-VALUE "DLGBGCOLOR"
		  :SYSTEM-DEFAULT "DLGBGCOLOR" :FLAGS NIL)
		 (:NAME "XFONTID" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P NIL :DEFAULT-VALUE NIL :SYSTEM-DEFAULT NIL
		  :FLAGS (:NO-INHERIT :NO-STRING :NOT-MAPPED))
		 (:NAME "PANGOFONTDESC" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P NIL :DEFAULT-VALUE NIL
		  :SYSTEM-DEFAULT NIL :FLAGS (:NO-INHERIT :NO-STRING :NOT-MAPPED))
		 (:NAME "DROPTARGET" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT
			NIL :FLAGS (:NO-INHERIT))
		 (:NAME "ENTERWINDOW_CB" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE ""
		  :SYSTEM-DEFAULT NIL :FLAGS (:NO-INHERIT :CALLBACK))
		 (:NAME "ALIGNMENT" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P T :DEFAULT-VALUE "ALEFT:ACENTER"
		  :SYSTEM-DEFAULT NIL :FLAGS (:NO-INHERIT))
		 (:NAME "DRAGSOURCE" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT
			NIL :FLAGS (:NO-INHERIT))
		 (:NAME "DROPDATA_CB" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE "sCiii"
		  :SYSTEM-DEFAULT NIL :FLAGS (:NO-INHERIT :CALLBACK))
		 (:NAME "MARKUP" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE NIL :SYSTEM-DEFAULT
			NIL :FLAGS NIL)
		 (:NAME "FLOATING" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT NIL
		  :FLAGS (:NO-INHERIT :NOT-MAPPED))
		 (:NAME "NORMALIZERGROUP" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P T :DEFAULT-VALUE NIL
		  :SYSTEM-DEFAULT NIL :FLAGS (:NO-INHERIT :NOT-MAPPED :IHANDLENAME))
		 (:NAME "PANGOLAYOUT" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P NIL :DEFAULT-VALUE NIL :SYSTEM-DEFAULT
			NIL :FLAGS (:NO-INHERIT :NO-STRING :NOT-MAPPED))
		 (:NAME "RASTERSIZE" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT
			NIL :FLAGS (:NO-INHERIT :NO-DEFAULTVALUE :NOT-MAPPED :NO-SAVE))
		 (:NAME "TIPRECT" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE NIL :SYSTEM-DEFAULT
			NIL :FLAGS (:NOT-MAPPED))
		 (:NAME "START" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT NIL
		  :FLAGS (:NO-INHERIT :NOT-MAPPED :WRITEONLY))
		 (:NAME "TIPFGCOLOR" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE "0 0 0"
		  :SYSTEM-DEFAULT "0 0 0" :FLAGS (:NOT-MAPPED))
		 (:NAME "FONTFACE" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT NIL
		  :FLAGS (:NO-INHERIT :NOT-MAPPED :NO-SAVE))
		 (:NAME "DRAGDATA_CB" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE "sCi"
		  :SYSTEM-DEFAULT NIL :FLAGS (:NO-INHERIT :CALLBACK))
		 (:NAME "NAME" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT NIL
		  :FLAGS (:NO-INHERIT :NO-DEFAULTVALUE :NOT-MAPPED))
		 (:NAME "ELLIPSIS" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT
			NIL :FLAGS NIL)
		 (:NAME "DRAGDATASIZE_CB" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE "s"
		  :SYSTEM-DEFAULT NIL :FLAGS (:NO-INHERIT :CALLBACK))
		 (:NAME "DROPFILES_CB" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE "siii"
		  :SYSTEM-DEFAULT NIL :FLAGS (:NO-INHERIT :CALLBACK))
		 (:NAME "ACTIVE" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P T :DEFAULT-VALUE "YES" :SYSTEM-DEFAULT
			"YES" :FLAGS NIL)
		 (:NAME "TIPVISIBLE" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT
			NIL :FLAGS (:NO-INHERIT))
		 (:NAME "ANIMATION_HANDLE" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P T :DEFAULT-VALUE NIL
		  :SYSTEM-DEFAULT NIL :FLAGS (:NO-INHERIT :NO-STRING :NOT-MAPPED :IHANDLE))
		 (:NAME "EXPANDWEIGHT" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE NIL
		  :SYSTEM-DEFAULT NIL :FLAGS (:NO-INHERIT :NOT-MAPPED))
		 (:NAME "MINSIZE" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P T :DEFAULT-VALUE "0x0" :SYSTEM-DEFAULT
			"0x0" :FLAGS (:NO-INHERIT :NOT-MAPPED))
		 (:NAME "UNMAP_CB" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE "" :SYSTEM-DEFAULT
			NIL :FLAGS (:NO-INHERIT :CALLBACK))
		 (:NAME "ANIMATION" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT NIL
		  :FLAGS (:NO-INHERIT :NOT-MAPPED :IHANDLENAME))
		 (:NAME "CHARSIZE" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P NIL :DEFAULT-VALUE NIL :SYSTEM-DEFAULT
			NIL :FLAGS (:NO-INHERIT :NO-DEFAULTVALUE :NOT-MAPPED :READONLY))
		 (:NAME "STOP" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT NIL
		  :FLAGS (:NO-INHERIT :NOT-MAPPED :WRITEONLY))
		 (:NAME "BUTTON_CB" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE "iiiis"
		  :SYSTEM-DEFAULT NIL :FLAGS (:NO-INHERIT :CALLBACK))
		 (:NAME "DRAGTYPES" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT
			NIL :FLAGS (:NO-INHERIT :NOT-MAPPED))
		 (:NAME "STOPWHENHIDDEN" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE "Yes"
		  :SYSTEM-DEFAULT "Yes" :FLAGS (:NO-INHERIT :NOT-MAPPED))
		 (:NAME "FONTSTYLE" :TYPE 0 :GET-FUNC-P T :SET-FUNC-P T :DEFAULT-VALUE NIL :SYSTEM-DEFAULT NIL
		  :FLAGS (:NO-INHERIT :NOT-MAPPED :NO-SAVE))
		 (:NAME "LEAVEWINDOW_CB" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P NIL :DEFAULT-VALUE ""
		  :SYSTEM-DEFAULT NIL :FLAGS (:NO-INHERIT :CALLBACK))
		 (:NAME "FONT" :TYPE 0 :GET-FUNC-P NIL :SET-FUNC-P T :DEFAULT-VALUE "DEFAULTFONT"
		  :SYSTEM-DEFAULT "DEFAULTFONT" :FLAGS (:NOT-MAPPED))))
  "IUP")

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
