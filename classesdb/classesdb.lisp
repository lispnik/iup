(in-package #:iup-classesdb)

(defparameter *modules*
  (list (list :initializer 'iup-controls:open)
	(list :initializer 'iup-glcontrols:open)
	(list :initializer 'iup-gl:open)
	(list :initializer 'iup-scintilla:open)
	(list :initializer 'iup-plot:open)
	(list :initializer 'iup-mglplot:open)
	(list :initializer 'iup-web:open :classname-excludes '("iupolecontrol"))
	#+windows (list :initializer 'iup-olecontrol:open)))

(loop with base-classnames = (iup:with-iup () (iup:all-classes))
      for module-metadata in *modules*
      for initializer = (getf module-metadata :initializer)
      for classes = (iup:with-iup () (funcall initializer) (iup:all-classes))
      for difference = (set-difference classes base-classnames :test #'string=)
      collect (list* :initializer initializer :classnames difference) into classnames
      finally (return (list* :initializer 'iup:open :classnames base-classnames classnames)))

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



