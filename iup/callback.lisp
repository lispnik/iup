(in-package #:iup)

(defun %make-key (name handle)
  (format nil "~A-~X"
          name
          handle))

(defvar *registered-callbacks* (make-hash-table :test 'equal))

(defun register-callback (name handle action)
  (setf (gethash (%make-key name handle) *registered-callbacks*)
        action))

(defun unregister-callback (name handle)
  (remhash (%make-key name handle) *registered-callbacks*))

(defun find-callback (name handle)
  (gethash (%make-key name handle) *registered-callbacks*))

(defun unregister-all-callbacks ()
  (clrhash *registered-callbacks*))
