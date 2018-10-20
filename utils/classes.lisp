(in-package #:iup)

(defun build-class-db (pathname)
  (with-open-file (stream pathname :direction :output :if-exists :supersede)
    (write 
     (mapcar #'(lambda (classname)
		 (flet ((keywords-to-symbols (keywords)
			  (mapcar #'(lambda (keyword)
				      (intern (symbol-name keyword)))
				  keywords)))
		   (cl:list :classname classname
			    :attributes (iup:class-attributes classname)
			    :callbacks (iup:class-callbacks classname))))
	     (iup:all-classes))
     :stream stream
     :pretty t
     :right-margin 80))
  (values))


(defparameter *metadata*
  '((:init iup-controls:open)
    (:init iup-glcontrols:open)
    (:init iup-gl:open)
    (:init iup-scintilla:open)
    (:init iup-plot:open)
    (:init iup-web:open)))

(defun class-metadata ()
  (flet ((keyword-to-symbol (keyword) (intern (symbol-name keyword))))
    (loop for classname in (iup:all-classes)
	  collect (cl:list  :classname classname
			    :attributes (iup:class-attributes classname)
			    :callbacks (iup:class-callbacks classname)))))

(print
 (loop with base-classnames = (iup:with-iup () (iup:all-classes))
       for opener in *openers*
       for classes = (iup:with-iup () (funcall opener) (iup:all-classes))
       for difference = (set-difference classes base-classnames :test #'string=)
       collect (list* :opener opener :classnames difference) into classnames
       finally (return (list* :opener 'iup:open :classnames base-classnames classnames)))
 )

#+nil
(with-iup ()
  (iup-controls:open)
  (iup-glcontrols:open)
  (iup-gl:open)
  (iup-scintilla:open)
  (iup-plot:open)
  (iup-web:open)
  (build-class-db #p "/home/mkennedy/Projects/local-projects/lispnik/iup/classes.lisp-sexp2"))


