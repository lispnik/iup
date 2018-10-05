(in-package #:iup)

;;; experimental

#|
(defun build-class-db (pathname)
  (with-open-file (stream pathname :direction :output :if-exists :supersede)
    (write 
     (with-iup
       (mapcar #'(lambda (classname)
		   (cl:list :classname classname
			    :attributes (get-class-attributes classname)
			    :callbacks (get-class-callbacks classname)))
	       (get-all-classes)))
     :stream stream
     :pretty t
     :right-margin 80))
  (values))
|#
