(in-package #:iup)

(defstruct callback
  name
  handle)

(ignore-errors
 (genhash:register-test-designator
  'callback=
  (lambda (callback)
    (sxhash (cl:list (callback-name callback)
		     (pffft:pointer (callback-handle callback)))))
  (lambda (a b)
    (and (eq (callback-name a)
	     (callback-name b))
	 (cffi:pointer-eq (pffft:pointer (callback-handle a))
			  (pffft:pointer (callback-handle b)))))))

(defvar *registered-callbacks* (genhash:make-generic-hash-table :test 'callback=))

(defun register-callback (name handle action)
  (check-type handle tecgraf-base:ihandle)
  (let ((callback (make-callback :name name :handle handle)))
    (setf (genhash:hashref callback *registered-callbacks*)
	  action)))

(defun unregister-callback (name handle)
  (check-type handle tecgraf-base:ihandle)
  (genhash:hashrem (make-callback :name name :handle handle) *registered-callbacks*))

(defun find-callback (name handle)
  (check-type handle tecgraf-base:ihandle)
  (genhash:hashref (make-callback :name name :handle handle) *registered-callbacks*))

(defun unregister-all-callbacks ()
  (genhash:hashclr *registered-callbacks*))
