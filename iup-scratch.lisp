
(defstruct event
  name
  handle 
  args)

(defstruct callback
  name
  handle)

(genhash:register-test-designator
 'callback=
 (lambda (callback)
   (sxhash (list (callback-name callback)
		 (callback-handle callback))))
 (lambda (a b)
   (and (eq (callback-name a)
	    (callback-name b))
	(cffi:pointer-eq (callback-handle a)
			 (callback-handle b)))))

(defvar *registered-callbacks* (genhash:make-generic-hash-table :test 'callback=))

(defun register-callback (name handle action)
  (check-type handle cffi:foreign-pointer)
  (setf (genhash:hashref (make-callback :name name :handle handle) *registered-callbacks*) action))

(defun unregister-callback (name handle)
  (check-type handle cffi:foreign-pointer)
  (genhash:hashrem (make-callback :name name :handle handle) *registered-callbacks*))

(defvar *event-queue* '())

(defun process-event (event)
  (let* ((handle (event-handle event))
	 (action (genhash:hashref (make-callback :name (event-name event) :handle handle)
				  *registered-callbacks*)))
    ;; TODO some customization of this might be required (eg. for drag and drop files)
    (apply action handle (event-args event))))

(defun process-events ()
  (dolist (event *event-queue*)
    (process-event event))
  (setf *event-queue* nil))

(cffi:defcallback action-callback :int ((handle :pointer))
  (pushnew (make-event :name :action :handle handle :args '())
	   *event-queue*)
  iup::+ignore+)

(defun button (&key title action)
  (let ((handle (iup-cffi::%iup-create "button")))
    (iup-cffi::%iup-set-str-attribute handle "TITLE" title)
    (register-callback :action handle action)
    (iup-cffi::%iup-set-callback handle "ACTION" (cffi:get-callback 'action-callback))
    handle))

(defun button-test ()
  (sb-int:with-float-traps-masked
      (:divide-by-zero :invalid)
    (iup:with-iup ()
      (let* ((button1 (button :title "Press Me!"
			      :action #'(lambda (handle)
					  (iup:message "Hello"
						       (format nil "Hello from Land of Lisp via ~S~%" handle)))))
	     (button2 (button :title "Close"
			      :action #'(lambda (handle)
					  (declare (ignore handle))
					  ;; FIXME
					  (iup:exit-loop))))
	     (hbox (iup:hbox (list button1 button2) :gap "10"))
	     (dialog (iup:dialog hbox)))
	(iup:show dialog)
	(loop
	   (process-events)
	   (iup:loop-step-wait))))))

