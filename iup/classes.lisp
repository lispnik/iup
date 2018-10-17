(in-package #:iup)

;;; experimental

(defun build-class-db (pathname)
  (with-open-file (stream pathname :direction :output :if-exists :supersede)
    (write 
     (mapcar #'(lambda (classname)
		 (cl:list :classname classname
		       :attributes (iup:class-attributes classname)
		       :callbacks (iup:class-callbacks classname)))
	     (iup:all-classes))
     :stream stream
     :pretty t
     :right-margin 80))
  (values))

#+nil
(with-iup ()
  (iup-controls:open)
  (iup-glcontrols:open)
  (iup-gl:open)
  (iup-scintilla:open)
  (iup-plot:open)
  (iup-web:open)
  (build-class-db #p "/Users/Matthew/quicklisp/local-projects/iup/classes.lisp-sexp"))


