(in-package #:iup-classesdb)

(autowrap:define-bitmask-from-enum (iattrib-flags iup-classesdb-cffi:iattrib-flags))

(defun attribute-metadata (class attrib table)
  (cffi:with-foreign-objects
      ((get-func :pointer)
       (set-func :pointer)
       (default-value :pointer)
       (system-default :pointer)
       (flags :int))
    (iup-classesdb-cffi.functions:iup-class-register-get-attribute
     class
     attrib
     get-func
     set-func
     default-value
     system-default
     flags)
    (list
     :type (iup-classesdb-cffi.functions:iup-table-get-curr-type table)
     :get-func-p (not (cffi:null-pointer-p (cffi:mem-ref get-func :pointer)))
     :set-func-p (not (cffi:null-pointer-p (cffi:mem-ref set-func :pointer)))
     :default-value (cffi:foreign-string-to-lisp (cffi:mem-aref default-value :pointer))
     :system-default (cffi:foreign-string-to-lisp (cffi:mem-aref system-default :pointer))
     :flags (autowrap:mask-keywords 'iattrib-flags (cffi:mem-ref flags :int)))))

(defun class-metadata (classname)
  (let* ((class (iup-classesdb-cffi.functions:iup-register-find-class classname))
	 (table (iup-classesdb-cffi.accessors:iclass-.attrib-func class)))
    (iup-classesdb-cffi.functions:iup-table-count table)
    (loop for attrib = (iup-classesdb-cffi.functions:iup-table-first table)
	    then (iup-classesdb-cffi.functions:iup-table-next table)
	  while attrib
	  collect (list* :name attrib (attribute-metadata class attrib table)))))

(defun classes-metadata ()
  (loop for classname in (iup:all-classes)
	collect
	(list :classname
	      classname
	      :attributes 	      
	      (class-metadata classname))))

(defparameter *static-metadata*
  '((:initializer iup:open
     :child-p ("submenu" "spinbox" "radio" "backgroundbox" "scrollbox" "flatscrollbox"
	       "detachbox" "expander" "sbox")
     :children-p ("menu" "cbox" "gridbox" "hbox" "vbox" "zbox" "normalizer" "frame"
		  "flatframe" "tabs" "flattabs" "split")
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
    (:initializer iup-controls:open
     :vanity-alist (("matrixex" . "matrix-ex")
		    ("matrixlist" . "matrix-list")))
    (:initializer iup-gl:open
     :child-p ("glbackgroundbox")
     :vanity-alist (("glcanvas" . "canvas")
		    ("glsubcanvas" . "sub-canvas")))
    (:initializer iup-glcontrols:open
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
    (:initializer iup-plot:open)
    (:initializer iup-mglplot:open
     :vanity-alist (("mglplot" . "plot")
		    ("mgllabel" . "label")))
    #+windows (list :initializer 'iup-olecontrol:open)
    (:initializer iup-scintilla:open
     :vanity-alist (("scintilladlg" . "scintilla-dialog")))
    (:initializer iup-web:open
     :classname-excludes '("olecontrol")
     :vality-alist (("webbrowser" . "web-browser"))))
  "Not everything about the IUP APIs can be extracted by
introspection. This describes the static metadata that is augmented
with IUP metadata upon introspection.")

(defun create-classesdb ()
  "Create a printable representaion of IUP metadata containing enough
information to create the Lisp API at compilation time."
  (flet ((sort-keywords (keywords)
	   (sort keywords #'string< :key #'symbol-name)))
    (loop with base-classnames = (iup:with-iup () (iup:all-classes))
	  for metadata in *static-metadata*
	  for initializer = (getf metadata :initializer)
	  for classes = (iup:with-iup () (funcall initializer) (iup:all-classes))
	  for classname-excludes = (getf metadata :classname-excludes)
	  for difference = (remove-if #'(lambda (classname)
					  (find classname classname-excludes :test #'string=))
				      (if (eq initializer 'iup:open)
					  base-classnames
					  (set-difference classes base-classnames :test #'string=)))
	  for children-p = (getf metadata :children-p)
	  for child-p = (getf metadata :child-p)
	  for override-p = (getf metadata :override-p)
	  for vanity-alist = (getf metadata :vanity-alist)
	  collect (iup:with-iup ()
		    (funcall initializer)
		    (list :package (package-name (symbol-package initializer))
			  :classnames
			  (loop for classname in difference
				collect (list :classname classname
					      :child-p (and (find classname child-p :test #'string=) t)
					      :children-p (and (find classname children-p :test #'string=) t)
					      :override-p (and (find classname override-p :test #'string=) t)
					      :vanity-classname (if-let (vanity-name (assoc-value vanity-alist classname :test #'string=))
								  (string-upcase vanity-name))
					      :attributes (sort-keywords (iup:class-attributes classname))
					      :callbacks (sort-keywords (iup:class-callbacks classname))))))
	    into result
	  finally (return (list* :platform (platform) :metadata result)))))

(iup:with-iup ()
  (print (classes-metadata)))

(defparameter *libraries*
  (list (list :initializer 'iup:open)
	(list :initializer 'iup-controls:open)
	(list :initializer 'iup-glcontrols:open)
	(list :initializer 'iup-gl:open)
	(list :initializer 'iup-scintilla:open)
	(list :initializer 'iup-plot:open)
	(list :initializer 'iup-mglplot:open)
	#+windows (list :initializer 'iup-olecontrol:open)
	(list :initializer 'iup-web:open :classname-excludes '("iupolecontrol"))
	#+windows (list :initializer 'iup-olecontrol:open)))

(loop with base-classnames = (iup:with-iup () (iup:all-classes))
      for module-metadata in *libraries*
      for initializer = (getf module-metadata :initializer)
      for classes = (iup:with-iup () (funcall initializer) (iup:all-classes))
      for classname-excludes = (getf module-metadata :classname-excludes)
      for difference = (remove-if #'(lambda (classname)
				      (find classname classname-excludes :test #'string=))
				  (if (eq initializer 'iup:open)
				      base-classnames
				      (set-difference classes base-classnames :test #'string=)))
      collect (list* :initializer initializer :classnames difference))





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
    (write classesdbs :stream stream :pretty t :right-margin 100)))

(defun regenerate ()
  (let* ((current-classesdbs (read-classesdbs))
	 (new-classesdb (create-classesdb))
	 (updated-classesdbs (update-classesdb current-classesdbs new-classesdb)))
    (write-classesdbs updated-classesdbs)))
