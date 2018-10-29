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
     :get-func-p (not (cffi:null-pointer-p (cffi:mem-ref get-func :pointer)))
     :set-func-p (not (cffi:null-pointer-p (cffi:mem-ref set-func :pointer)))
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

;;; iup/classesdb depends on IUP FFI, but can't use the one provided by the iup system
;;; because that creates a circular dependency. We don't break IUP's FFI into a seperate
;;; system because it provides too much of the API. Lastly, we us so little of the FFI in
;;; iup/classesdb that write our own simple wrappers here.

(defparameter *static-metadata*
  '((:initializer (%libiup "IupOpen")
     :child-p ("submenu" "spinbox" "radio" "backgroundbox" "scrollbox" "flatscrollbox" "detachbox" "expander" "sbox" "dialog")
     :children-p ("menu" "cbox" "gridbox" "hbox" "vbox" "zbox" "normalizer" "frame" "flatframe" "tabs" "flattabs" "split")
     :override-p ("image" "imagergb" "imagergba")
     :vanity-alist (("gridbox" . "grid-box")
		    ("flatframe" . "flat-frame")
		    ("flattabs" . "flat-tabs")
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
		    ("parambox" . "param-box")))
    (:initializer (%libiup-controls"IupControlsOpen")
     :vanity-alist (("matrixex" . "matrix-ex")
		    ("matrixlist" . "matrix-list")))
    (:initializer (%libiup-gl "IupGLCanvasOpen")
     :child-p ("glbackgroundbox")
     :vanity-alist (("glcanvas" . "canvas")
		    ("glsubcanvas" . "sub-canvas")))
    (:initializer (%libiup-glcontrols "IupGLControlsOpen")
     :child-p ("glexpander" "glframe" "glscrollbox" "glsizebox")
     :children-p ("glcanvasbox")
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
    (:initializer (%libiup-plot "IupPlotOpen"))
    (:initializer (%libiup-mglplot "IupMglPlotOpen")
     :vanity-alist (("mglplot" . "plot")
		    ("mgllabel" . "label")))
    #+windows (:initializer (%libiup-olecontrol "IupOleControlOpen"))
    (:initializer (%libiup-scintilla "IupScintillaOpen")
     :vanity-alist (("scintilladlg" . "scintilla-dialog")))
    (:initializer (%libiup-web "IupWebBrowserOpen")
     :classname-excludes ("olecontrol")
     :vality-alist (("webbrowser" . "web-browser"))))
  "Not everything about the IUP APIs can be extracted by
introspection. This describes the static metadata that is augmented
with IUP metadata upon introspection.")

(defun platform ()
  #+windows :windows
  #+linux :linux
  #+(and unix (not linux)) :unix)

(defun create-classesdb ()
  "Create a printable representaion of IUP metadata containing enough
information to create the Lisp API at compilation time."
  (flet ((vanity-name (vanity-alist classname)
	   (if-let (vanity-name (assoc-value vanity-alist classname :test #'string=))
	     (string-upcase vanity-name))))
    (loop with base-classnames = (with-iup (all-classes))
	  for metadata in *static-metadata*
	  for initializer = (getf metadata :initializer)
	  for classes = (with-iup (funcall initializer) (all-classes))
	  for classname-excludes = (getf metadata :classname-excludes)
	  for difference = (remove-if #'(lambda (classname)
					  (find classname classname-excludes :test #'string=))
				      (if (eq initializer 'iup-cffi::%iup-open)
					  base-classnames
					  (set-difference classes base-classnames :test #'string=)))
	  for children-p = (getf metadata :children-p)
	  for child-p = (getf metadata :child-p)
	  for override-p = (getf metadata :override-p)
	  for vanity-alist = (getf metadata :vanity-alist)
	  collect
	  (with-iup 
	    (funcall initializer)
	    (list :package (package-name (symbol-package initializer))
		  :classnames
		  (loop for classname in difference
			collect
			(list :classname classname
			      :child-p (and (find classname child-p :test #'string=) t)
			      :children-p (and (find classname children-p :test #'string=) t)
			      :override-p (and (find classname override-p :test #'string=) t)
			      :vanity-classname (vanity-name vanity-alist classname)
			      :attributes (class-metadata classname)))))
	    into result
	  finally (return (list* :platform
				 (platform)
				 :metadata result)))))

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
  (let ((our-platform (platform)))
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
