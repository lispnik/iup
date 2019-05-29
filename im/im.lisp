(defpackage #:iup-im
  (:use #:common-lisp)
  (:export #:im-error
           #:im-error-message
           #:load-image
           #:save-image
           #:load-animation
           #:load-animation-frames
           #:native-handle-im-image
           #:im-image-native-handle
           #:image-from-im-image
           #:image-to-im-image))

(in-package #:iup-im)

(define-condition im-error (error)
  ((message :initarg :message
            :type string
            :reader im-error-message))
  (:report (lambda (condition stream)
             (format stream "IM error: ~A" (im-error-message condition)))))

(defun last-error () (iup-cffi::%iup-get-global "IUPIM_LASTERROR"))

(defun load-image (pathspec)
  (let* ((filename (namestring (translate-logical-pathname pathspec)))
         (result (iup-im-cffi::%iup-im-load-image filename)))
    (or result (cerror "IM error" 'im-error  :message (last-error)))))

(defun save-image (handle pathspec format)
  (let* ((filename (namestring (translate-logical-pathname pathspec)))
         (result (iup-im-cffi::%iup-im-save-image handle filename format)))
    (unless result
      (cerror "IM error" 'im-error :message (last-error)))
    (values)))

(defun load-animation (pathspec)
  (let* ((filename (namestring (translate-logical-pathname pathspec)))
         (result (iup-im-cffi::%iup-im-load-animation filename)))
    (or result (cerror "IM error" 'im-error  :message (last-error)))))

(defun load-animation-frames (pathspec-list)
  (check-type pathspec-list list)
  (let* ((filename-list (mapcar #'(lambda (pathspec)
                                    (truename (translate-logical-pathname pathspec)))
                                pathspec-list))
         (filename-count (length filename-list)))
    (cffi:with-foreign-object (filename-ptrs :pointer filename-count)
      (let ((allocated '()))
        (unwind-protect
             (loop for filename in filename-list
                   for filename-ptr = (cffi:foreign-string-alloc filename)
                   for i from 0
                   do (push filename-ptr allocated)
                   do (setf (cffi:mem-aref filename-ptrs :pointer i) filename-ptr)
                   finally
                      (return (iup-im-cffi::%iup-im-load-animation-frames filename-ptrs filename-count)))
          (dolist (alloc allocated)
            (cffi:foreign-free alloc)))))))

(defun native-handle-im-image (native-handle)
  (iup-im-cffi::%iup-im-get-native-handle-image native-handle))

(defun im-image-native-handle (im-image)
  (iup-im-cffi::%iup-im-get-image-native-handle im-image))

(defun image-from-im-image (im-image)
  (iup-im-cffi::%iup-im-iup-image-from-im-image im-image))

(defun image-to-im-image (image)
  (iup-im-cffi::%iup-im-iup-image-to-im-image image))
