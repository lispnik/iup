(defpackage #:iup-classesdb
  (:use #:common-lisp
	#:alexandria)
  (:export #:regenerate))

(in-package #:iup-classesdb)

(defun attribute-metadata (class attrib table)
  (cffi:with-foreign-objects
      ((get-func :pointer)
       (set-func :pointer)
       (default-value :pointer)
       (system-default :pointer)
       (flags :int))
    (iup-classesdb-cffi::%iup-class-register-get-attribute
     class
     attrib
     get-func
     set-func
     default-value
     system-default
     flags)
    (list
     :type (iup-classesdb-cffi::%iup-table-get-curr-type table)
     :default-value (cffi:foreign-string-to-lisp (cffi:mem-aref default-value :pointer))
     :system-default (cffi:foreign-string-to-lisp (cffi:mem-aref system-default :pointer))
     :flags (cffi:foreign-bitfield-symbols 'iup-classesdb-cffi::attrib-flags (cffi:mem-ref flags :int)))))

(defun attribute-table (class)
  (cffi:with-foreign-slots ((iup-classesdb-cffi::attrib-func) class (:struct iup-classesdb-cffi::iclass))
    iup-classesdb-cffi::attrib-func))

(defun class-metadata (classname)
  (let* ((class (iup-classesdb-cffi::%iup-register-find-class classname))
	 (table (attribute-table class)))
    (loop for attrib = (iup-classesdb-cffi::%iup-table-first table)
	    then (iup-classesdb-cffi::%iup-table-next table)
	  while attrib
	  collect (list* :name attrib (attribute-metadata class attrib table)))))

(defun all-classes ()
  (flet ((iup-get-all-classes (names n)
	   (iup-cffi::%iup-get-all-classes names n)))
    (let* ((max-n (iup-get-all-classes (cffi:null-pointer) 0))
	   (array (cffi:foreign-alloc :pointer
				      :initial-element (cffi:null-pointer)
				      :count max-n
				      :null-terminated-p t)))
      (unwind-protect
	   (progn
	     (iup-get-all-classes array max-n)
	     (loop for i below max-n
		   for ref = (cffi:mem-aref array :pointer i)
		   until (cffi:null-pointer-p ref)
		   collect (cffi:foreign-string-to-lisp ref) into result
		   finally (return (sort result #'string<))))
	(cffi:foreign-free array)))))

(defmacro with-iup (&body body)
  (with-gensyms (result)
    `(unwind-protect
	  (let ((,result (iup-cffi::%iup-open (cffi:null-pointer) (cffi:null-pointer))))
	    (unless (zerop ,result)
	      (error "Can't load IUP"))
	    ,@body)
       (iup-cffi::%iup-close))))

(defun classes-metadata ()
  (loop for classname in (all-classes)
	collect
	(list :classname
	      classname
	      :attributes 	      
	      (class-metadata classname))))

(defparameter *static-metadata*
  '((:initializer iup-cffi::%iup-open
     :package "IUP"
     :override-p ("image" "imagergb" "imagergba")
     :vanity-alist (("gridbox" . "grid-box")
		    ("flatframe" . "flat-frame")
		    ("flattabs" . "flat-tabs")
		    ("flatlist" . "flat-list")
		    ("backgroundbox" . "background-box")
		    ("scrollbox" . "scroll-box")
		    ("flatscrollbox" . "flat-scroll-box")
		    ("detachbox" . "detach-box")
		    ("animatedlabel" . "animated-label")
		    ("flatbutton" . "flat-button")
		    ("dropbutton" . "drop-button")
		    ("colorbrowser" . "color-browser")
		    ("datepick" . "date-pick")
		    ("flatlabel" . "flat-label")
		    ("flatseparator" . "flat-separator")
		    ("progressbar" . "progress-bar")
		    ("flattoggle" . "flat-toggle")
		    ("multiline" . "multi-line")
		    ("submenu" . "sub-menu")
		    ("colordlg" . "color-dialog")
		    ("filedlg" . "file-dialog")
		    ("fontdlg" . "font-dialog")
		    ("messagedlg" . "message-dialog")
		    ("progressdlg" . "progress-dialog")
		    ("parambox" . "param-box")
		    ("imagergb" . "image-rgb")
		    ("imagergba" . "image-rgba")
		    ("multibox" . "multi-box")))
    (:initializer iup-controls-cffi::%iup-controls-open
     :package "IUP-CONTROLS"
     :vanity-alist (("matrixex" . "matrix-ex")
		    ("matrixlist" . "matrix-list")))
    (:initializer iup-gl-cffi::%iup-gl-canvas-open
     :package "IUP-GL"
     :vanity-alist (("glcanvas" . "canvas")
		    ("glsubcanvas" . "sub-canvas")))
    (:initializer iup-glcontrols-cffi::%iup-glcontrols-open
     :package "IUP-GLCONTROLS"
     :vanity-alist (("glcanvasbox" . "canvas-box")
		    ("glsubcanvas" . "sub-canvas")
		    ("glprogressbar" . "progress-bar")
		    ("glscrollbox" . "scroll-box")
		    ("glsizebox" . "size-box")
		    ("glval" . "val")
		    ("gltoggle" . "toggle")	
		    ("gltext" . "text")
		    ("gllink" . "link")	
		    ("glframe" . "frame")
		    ("glexpander" . "expander")	
		    ("glbutton" . "button")	
		    ("gllabel" . "label")
		    ("glseparator" . "separator")
		    ("glbackgroundbox" . "background-box")))
    (:initializer iup-plot-cffi::%iup-plot-open
     :package "IUP-PLOT")
    (:initializer iup-mglplot-cffi::%iup-mglplot-open
     :package "IUP-MGLPLOT"
     :vanity-alist (("mglplot" . "plot")
		    ("mgllabel" . "label")))
    #+windows (:initializer iup-olecontrol-cffi::%iup-olecontrol-open
	       :package "IUP-OLECONTROL")
    (:initializer iup-scintilla-cffi::%iup-scintilla-open
     :package "IUP-SCINTILLA"
     :vanity-alist (("scintilladlg" . "scintilla-dialog")))
    (:initializer iup-web-cffi::%iup-web-browser-open
     :package "IUP-WEB"
     :classname-excludes ("olecontrol")
     :vanity-alist (("webbrowser" . "web-browser")))
    (:initializer iup-tuio-cffi::%iup-tuio-open
     :package "IUP-TUIO"
     :override-p ("tuioclient")
     :vanity-alist (("tuioclient" . "client"))))
  "Information on how to create the Lisp bindings.

:INITIALIZER function to call which initializes a specific IUP library
:PACKAGE the name of a package from which the Lisp bindings should be export
:CLASSNAME-EXCLUDES a list of IUP class names to exclude

:OVERRIDE-P list of IUP class names which should not be created
automatically (e.g. because they require a specific argument lists at
creation)

:VANITY-ALIST a mapping between IUP names and Lisp names")

(defparameter *platform* 
  #+windows :windows
  #+linux :linux
  #+(and unix (not linux)) :unix)

(defun class-format (class)
  (cffi:with-foreign-slots ((iup-classesdb-cffi::format) class (:struct iup-classesdb-cffi::iclass))
    iup-classesdb-cffi::format))

(defun class-child-type (class)
  (cffi:with-foreign-slots ((iup-classesdb-cffi::child-type) class (:struct iup-classesdb-cffi::iclass))
    iup-classesdb-cffi::child-type))

(defun child-spec-from-format (format)
  "Returns :CHILD-NONE, :CHILD-MANY or an integer count of children."
  (cond ((find #\g format)
	 :child-many)
	((find #\h format)
	 (count #\h format))
	(t :child-none)))

(defun create-classesdb ()
  "Create a printable representaion of IUP metadata containing enough
information to create the Lisp API at compilation time."
  (flet ((vanity-name (vanity-alist classname)
	   (if-let (vanity-name (assoc-value vanity-alist classname :test #'string=))
	     (string-upcase vanity-name))))
    (loop with base-classnames = (with-iup (all-classes))
	  for metadata in *static-metadata*
	  for initializer = (getf metadata :initializer)
	  for classes = (with-iup (if (eq initializer 'iup-cffi::%iup-open)
				      (funcall initializer (cffi:null-pointer) (cffi:null-pointer))
				      (funcall initializer))
			  (all-classes))
	  for classname-excludes = (getf metadata :classname-excludes)
	  for difference = (remove-if #'(lambda (classname)
					  (find classname classname-excludes :test #'string=))
				      (if (eq initializer 'iup-cffi::%iup-open)
					  base-classnames
					  (set-difference classes base-classnames :test #'string=)))
	  for override-p = (getf metadata :override-p)
	  for vanity-alist = (getf metadata :vanity-alist)
	  for package = (getf metadata :package)
	  do (format t "~&Processing for package ~A" package)
	  collect
	  (with-iup 
	      (if (eq initializer 'iup-cffi::%iup-open)
		  (funcall initializer (cffi:null-pointer) (cffi:null-pointer))
		  (funcall initializer))
	    (list :package package
		  :classnames
		  (loop for classname in difference
			for class = (iup-classesdb-cffi::%iup-register-find-class classname)
			for class-format = (class-format class)
			for class-child-type = (class-child-type class)
			collect
			(list :classname classname
			      :format class-format
			      :children (child-spec-from-format class-format)
			      :override-p (and (find classname override-p :test #'string=) t)
			      :vanity-classname (vanity-name vanity-alist classname)
			      :attributes (class-metadata classname)))))
	    into result
	  finally (return (append (list :platform *platform*)
				  (list :metadata result))))))

(defun classesdb-pathname ()
  (asdf:system-relative-pathname "iup" "classesdb" :type "lisp-sexp"))

(defun read-classesdbs ()
  (let ((classesdb-pathname (classesdb-pathname)))
    (if (probe-file classesdb-pathname)
	(with-open-file (stream classesdb-pathname :direction :input)
	  (let ((*read-eval* nil))
	    (read stream stream)))
	'((:platform :linux)
	  (:platform :windows)
	  (:platform :unix)))))

(defun update-classesdbs (current-classesdbs classesdb)
  (let ((our-platform *platform*))
    (mapcar #'(lambda (existing-classesdb)
		(if (eq (getf existing-classesdb :platform) our-platform)
		    classesdb
		    existing-classesdb))
	    current-classesdbs)))

(defun write-classesdbs (classesdbs)
  (with-open-file (stream (classesdb-pathname) :direction :output :if-exists :supersede)
    (format stream ";;; generated at ~A for IUP ~A -*-lisp-*-~%~%"
	    (local-time:format-timestring nil (local-time:universal-to-timestamp (get-universal-time))
					  :timezone local-time:+utc-zone+)
	    (with-iup (iup-cffi::%iup-version)))
    (write classesdbs :stream stream :pretty t :right-margin 100)))

(defun regenerate ()
  (let* ((current-classesdbs (read-classesdbs))
	 (new-classesdb (create-classesdb))
	 (updated-classesdbs (update-classesdbs current-classesdbs new-classesdb)))
    (write-classesdbs updated-classesdbs)
    nil))
