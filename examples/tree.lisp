(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "iup-controls" "uiop")))

(defpackage #:iup-examples.tree
  (:use #:common-lisp)
  (:export #:tree))

(in-package #:iup-examples.tree)

(defun get-dir (pathname)
  (assert (uiop:directory-pathname-p pathname))
  (loop for pathname in (uiop:directory* (make-pathname :name :wild :defaults pathname))
	if (uiop:directory-pathname-p pathname)
	  collect pathname into dirs
	else
	  collect pathname into files
	finally (return (values dirs files))))

(defun fill-tree (tree id pathname)
  (multiple-value-bind
	(dirs files)
      (get-dir pathname)
    (dolist (file files)
      (setf (iup:attribute tree :addleaf) (namestring file)))
    (dolist (dir dirs)
      (setf (iup:attribute tree :addbranch) (namestring dir)))
    (setf (iup:attribute tree :title) (namestring pathname))))

(defun map-callback (handle)
  (fill-tree handle 0 "/")
  iup:+default+)

(defun branchopen-callback (handle id)
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
