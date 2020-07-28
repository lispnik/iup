(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "bordeaux-threads" "lparallel" "log4cl")))

(in-package #:iup)

(defvar *post-message-handler* nil)

(defvar *post-message-queue* nil)

(defun post-message-callback (&rest args)
  (declare (ignore args))
  (loop until (lparallel.queue:queue-empty-p *post-message-queue*)
        for message = (lparallel.queue:try-pop-queue *post-message-queue*)
        do (funcall message))
    iup:+default+)

(defun iup-loop ()
  (iup:with-iup ()
    (setf *post-message-handler* (iup:user :postmessage_cb 'post-message-callback))
    (setf (iup:global :lockloop) :yes)
    (main-loop)))

(defvar *post-message-lock* (bt:make-lock))

(defun post-message ()
  (bt:with-lock-held (*post-message-lock*)
    (iup-cffi::%iup-post-message
     *post-message-handler* (cffi:null-pointer) 42 pi (cffi:null-pointer))))

(defun call-with-main-loop (func)
  (lparallel.queue:push-queue func *post-message-queue*)
  (post-message))

(defun start ()
  (when *post-message-handler* (exit-loop))
  (setf *post-message-handler* nil
        *post-message-queue* (lparallel.queue:make-queue))
  (bt:make-thread
   (lambda ()
     #+sbcl (sb-int:with-float-traps-masked
                (:divide-by-zero :invalid)
              (iup-loop))
     #-sbcl (iup-loop))
   :name "iup-main-loop"))

(defun stop ()
  (when *post-message-queue*
    (call-with-main-loop #'iup:exit-loop))
  (setf *post-message-queue* nil))

#+nil
(call-with-main-loop
 (lambda ()
   (let* ((text (iup:text :expand :yes :multiline :yes))
          (vbox (iup:vbox (list text)))
          (dialog (iup:dialog vbox :size "QUARTERxQUARTER")))
     (iup:show dialog))))

#+nil
(progn
  (trace post-message-callback)
  (stop)
  (sleep 0.2)
  (start)
  (sleep 0.2))

#+nil
(progn
  (defparameter text nil)
  (setf text (iup:text :multiline :yes :expand :yes))
  (let* ((vbox (iup:vbox (cl:list text)))
         (dialog (iup:dialog vbox :size "600x200")))
    (call-with-main-loop
     (lambda () (show dialog)))
    (sleep 2)
    (call-with-main-loop
     (lambda ()
       (loop for i from 0 below 10
             do (setf (iup:attribute text :append)
                      (format nil "Some text ~a" i)))))))

#+nil (start)

