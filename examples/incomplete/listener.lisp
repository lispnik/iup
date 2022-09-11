(ql:quickload '("iup" "iup-threads" "bordeaux-threads" "trivial-gray-streams"))

(defpackage #:iup-examples.listener
  (:use #:common-lisp)
  (:local-nicknames (#:gray #:trivial-gray-streams))
  (:export #:listener))

(in-package #:iup-examples.listener)

;;; porting from cocoa-ide/cocoa-listener.lisp from ccl

(defclass double-output-buffer ()
  ((flush-limit :initarg :flush-limit
                :accessor dob-flush-limit)
   (data :initarg :data
         :accessor dob-data)
   (other-data :initform nil
               :accessor dob-other-data)
   (output-data :initarg :output-data
                :accessor dob-output-data)
   (data-lock :initform (bt:make-recursive-lock)
              :accessor dob-data-lock)
   (output-data-lock :initform (bt:make-recursive-lock)
                     :accessor dob-output-data-lock)
   (semaphore :initform (bt:make-semaphore)
              :accessor dob-semaphore)))

(defparameter $listener-flush-limit 4095)

(defun make-double-output-buffer (&optional (flush-limit $listener-flush-limit))
  (check-type flush-limit (integer 0))
  (flet ((make-buffer ()
	   (make-array (1+ flush-limit)
		       :adjustable t
		       :fill-pointer 0
		       :element-type 'character)))
    (let* ((data (make-buffer))
	   (output-data (make-buffer))
	   (res (make-instance 'double-output-buffer
			       :flush-limit flush-limit
			       :data data
			       :output-data output-data)))
      (dob-return-output-data res)
      res)))

(defmacro with-dob-data ((data dob) &body body)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk (,data)
	      ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-dob-data #',thunk ,dob))))

;; The GUI thread isn't allowed to print on a listener output-stream,
;; so ignore all attempts.
(defun call-with-dob-data (thunk dob)
  (unless (eq sb-thread:*current-thread* iup-threads:*iup-main-loop-thread*)
    (bt:with-lock-held ((dob-data-lock dob))
      (funcall thunk (dob-data dob)))))

(defmacro with-dob-output-data ((data dob) &body body)
  (let ((thunk (gensym "THUNK")))
    `(flet ((,thunk (,data)
	      ,@body))
       (declare (dynamic-extent #',thunk))
       (call-with-dob-output-data #',thunk ,dob))))

(defun call-with-dob-output-data (thunk dob)
  (bt:with-lock-held ((dob-output-data-lock dob))
    (funcall thunk (dob-output-data dob))))

;; Should be called only in the GUI thread, except when
;; initializing a new double-output-buffer instance (or
;; debugging the semaphore wait code).
(defun dob-return-output-data (dob)
  (with-dob-output-data (output-data dob)
    (when output-data
      (setf (fill-pointer output-data) 0)
      (setf (dob-output-data dob) nil
	    (dob-other-data dob) output-data)
      (bt:signal-semaphore (dob-semaphore dob))
      output-data)))

;; Must be called inside WITH-DOB-DATA
(defun dob-queue-output-data (dob &optional force)
  (unless (and (not force) (eql 0 (length (dob-data dob))))
    (bt:wait-on-semaphore (dob-semaphore dob))
    (when (dob-other-data dob)
      (setf (dob-output-data dob) (dob-data dob)
	    (dob-data dob) (dob-other-data dob)
	    (dob-other-data dob) nil)
      t)))

;; True return means we overflowed the current buffer
(defun dob-push-char (dob char)
  (with-dob-data (data dob)
    (when (>= (vector-push-extend char data) (dob-flush-limit dob))
      (dob-queue-output-data dob t)
      t)))

(defclass listener-output-stream (gray:fundamental-character-output-stream)
  ((buffer :initform (make-double-output-buffer $listener-flush-limit))
   (view :initarg :view)))

(defmethod stream-element-type ((stream listener-output-stream))
  (with-slots (buffer) stream
    (array-element-type (dob-data buffer))))


(defun display-listener-output-buffer (stream)
  (with-slots (view buffer) stream
    (unwind-protect
	 (with-dob-output-data (data buffer)
	   (when (> (fill-pointer data) 0)
	     (setf (iup:attribute view :append) data)
	     (setf (fill-pointer data) 0)))
      (dob-return-output-data buffer))))

(defmethod gray:stream-write-char ((stream listener-output-stream) char)
  (with-slots (buffer) stream
    (when (dob-push-char buffer char)
      (queue-for-gui                    ;FIXME ?
       (lambda () (display-listener-output-buffer stream))))))

(defmethod gray:stream-clear-output ((stream listener-output-stream))
  (with-slots (buffer) stream
    (with-dob-data (data buffer)
      (setf (fill-pointer data) 0))))

;; (defmethod ccl:stream-line-length ((stream listener-output-stream))
;;   (with-slots (view) stream
;;     (values (hemlock-view-size hemlock-view))))

;; (defloadvar *cocoa-listener-count* 0)
(defvar *cocoa-listener-count* 0)

(defun listener ()
  (iup:with-iup ()
    (iup:main-loop)))

#-sbcl (listener)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (listener))
