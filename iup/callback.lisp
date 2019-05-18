(in-package #:iup)

(defstruct callback
  name
  handle)

(ignore-errors
 (genhash:register-test-designator
  'callback=
  (lambda (callback)
    (sxhash (cl:list (callback-name callback)
		     (callback-handle callback))))
  (lambda (a b)
    (and (eq (callback-name a)
	     (callback-name b))
	 (cffi:pointer-eq (callback-handle a)
			  (callback-handle b))))))

(defvar *registered-callbacks* (genhash:make-generic-hash-table :test 'callback=))

(defun register-callback (name handle action)
  (check-type handle cffi:foreign-pointer)
  (let ((callback (make-callback :name name :handle handle)))
    (setf (genhash:hashref callback *registered-callbacks*)
	  action)))

(defun unregister-callback (name handle)
  (check-type handle cffi:foreign-pointer)
  (genhash:hashrem (make-callback :name name :handle handle) *registered-callbacks*))

(defun find-callback (name handle)
  (check-type handle cffi:foreign-pointer)
  (genhash:hashref (make-callback :name name :handle handle) *registered-callbacks*))
