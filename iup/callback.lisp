(in-package #:iup)

(defun %make-key (name handle)
  (format nil "~A-~X"
          name
          (cffi:pointer-address (pffft:pointer handle))))

(defvar *registered-callbacks* (make-hash-table :test 'equal))

(defun register-callback (name handle action)
  (check-type handle tecgraf-base:ihandle)
  (setf (gethash (%make-key name handle) *registered-callbacks*)
        action))

(defun unregister-callback (name handle)
  (check-type handle tecgraf-base:ihandle)
  (remhash (%make-key name handle) *registered-callbacks*))

(defun find-callback (name handle)
  (check-type handle tecgraf-base:ihandle)
  (gethash (%make-key name handle) *registered-callbacks*))

(defun unregister-all-callbacks ()
  (clrhash *registered-callbacks*))
