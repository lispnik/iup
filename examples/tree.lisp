(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "uiop")))

(defpackage #:iup-examples.tree
  (:use #:common-lisp)
  (:export #:tree))

(in-package #:iup-examples.tree)

(defun get-dir (pathname)
  (when (uiop:directory-pathname-p pathname)
    (loop for pathname in (uiop:directory* (make-pathname :name :wild :defaults pathname))
	  if (uiop:directory-pathname-p pathname)
	    collect pathname into dirs
	  else
	    collect pathname into files
	  finally (return (values dirs files)))))

(defun fill-tree (tree id pathname)
  (multiple-value-bind
	(dirs files)
      (get-dir pathname)
    (flet ((sort-reversed (pathnames)
	     (sort pathnames #'string> :key #'namestring)))
      (setf (iup:attribute tree :addexpanded) :yes)
      (dolist (file (sort-reversed files))
	(setf (iup:attribute tree :addleaf) (namestring file)))
      (dolist (dir (sort-reversed dirs))
	(setf (iup:attribute tree :addbranch) (namestring dir))))
    (setf (iup:attribute tree :title) (namestring pathname))))

(defun map-callback (handle)
  (fill-tree handle 0 "/")
  iup:+default+)

(defun branchopen-callback (handle id)
  (format t "branchopen-callback ~S~%" (list handle id))
  (finish-output)
  (setf (iup:attribute handle (format nil "DELNODE~A" id)) "CHILDREN")
  (fill-tree handle id (iup:attribute handle (format nil "TITLE~A" id))) 
  iup:+default+)

(defun tree ()
  (iup:with-iup ()
    (let* ((tree (iup:tree :minsize "200x300"
			   :map_cb 'map-callback
			   :branchopen_cb 'branchopen-callback))
	   (dialog (iup:dialog tree :title "Tree Example")))
      (iup:show dialog)
      (iup:main-loop))))

#-sbcl (tree)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (tree))
