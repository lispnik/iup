(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "uiop")))

(defpackage #:iup-examples.tree-directory
  (:use #:common-lisp)
  (:export #:tree-directory))

(in-package #:iup-examples.tree-directory)

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
      (dolist (dir (sort-reversed dirs))
	(setf (iup:attribute tree :addbranch) (namestring dir)))
      (dolist (file (sort-reversed files))
	(setf (iup:attribute tree :addleaf) (namestring file))))
    (setf (iup:attribute tree :title) (namestring pathname))))

(defun map-callback (handle)
  (fill-tree handle 0 "/")
  iup:+default+)

(defun selection-callback (&rest args)
  (format t "selection-callback ~S~%" args)
  (force-output)
  iup:+default+)

(defun branchopen-callback (handle id)
  (format t "branchopen-callback ~S~%" (list handle id))
  (finish-output)
  (setf (iup:attribute handle (format nil "DELNODE~A" id)) "CHILDREN")
  (fill-tree handle id (iup:attribute handle (format nil "TITLE~A" id))) 
  iup:+default+)

(defun tree-directory ()
  (iup:with-iup ()
    (let* ((tree (iup:tree :minsize "200x300"
			   :map_cb 'map-callback
			   :branchopen_cb 'branchopen-callback
			   :selection_cb 'selection-callback))
	   (dialog (iup:dialog tree :title "Tree Example")))
      (iup:show dialog)
      (iup:main-loop))))

#-sbcl (tree-directory)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (tree-directory))
