(in-package #:iup-classesdb)

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

(defun build-classesdb (pathname)
  (with-open-file (stream pathname :direction :output :if-exists :supersede)
    (write 
     (mapcar #'(lambda (classname)
		 (flet ((keywords-to-symbols (keywords)
			  (mapcar #'(lambda (keyword)
				      (intern (symbol-name keyword)))
				  keywords)))
		   (list :classname classname
			 :attributes (iup:class-attributes classname)
			 :callbacks (iup:class-callbacks classname))))
	     (iup:all-classes))
     :stream stream
     :pretty t
     :right-margin 80))
  (values))

(defun class-metadata ()
  (flet ((keyword-to-symbol (keyword) (intern (symbol-name keyword))))
    (loop for classname in (iup:all-classes)
	  collect (cl:list  :classname classname
			    :attributes (iup:class-attributes classname)
			    :callbacks (iup:class-callbacks classname)))))




#+nil
(with-iup ()
  (iup-controls:open)
  (iup-glcontrols:open)
  (iup-gl:open)
  (iup-scintilla:open)
  (iup-plot:open)
  (iup-web:open)
  (build-class-db #p "/home/mkennedy/Projects/local-projects/lispnik/iup/classes.lisp-sexp2"))



