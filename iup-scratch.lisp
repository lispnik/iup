
(defstruct event
  event
  handle 
  args)

(defstruct event-mapping
  event
  handle)

(defvar *event-mappings*  (make-hash-table :test #'equalp))

(defvar *event-queue* '())

(cffi:defcallback action-callback :int ((handle :pointer))
  (pushnew (make-event :event :action :handle (cffi:pointer-address handle) :args '())
	   *event-queue*)
  iup::+default+)

(defun button (&key title action)
  (let ((handle (iup-cffi::%iup-create "button")))
    (iup-cffi::%iup-set-str-attribute handle "TITLE" title)
    (setf (gethash (make-event-mapping :event :action :handle (cffi:pointer-address handle)) *event-mappings*)
	  action)
    (iup-cffi::%iup-set-callback handle "ACTION" (cffi:get-callback 'action-callback))
    handle))

(defun process-events ()
  (loop for e in *event-queue*
	for event = (event-event e)
	for handle = (event-handle e)
	for args = (event-args e)
	for action = (gethash (make-event-mapping :event event :handle handle) *event-mappings*)
	when action
	  do (apply action
		    handle args))
  (setf *event-queue* nil))

(defun button-test ()
  (sb-int:with-float-traps-masked
      (:divide-by-zero :invalid)
    (iup:with-iup ()
      (let* ((button (button :title "Press Me!"
			     :action #'(lambda (handle)
					 (format t "Hello from Lisp land via ~S~%" handle)
					 iup::+default+)))
	     (hbox (iup:hbox (list button)))
	     (dialog (iup:dialog hbox)))
	(iup:show dialog)
	(loop 
	  (process-events)
	  (iup:loop-step-wait))))))

