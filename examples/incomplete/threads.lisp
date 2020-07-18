(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "bordeaux-threads" "lparallel")))

(in-package #:iup)

(defvar *post-message-handler* nil)

(defvar *post-message-queue* (lparallel.queue:make-queue))

(defun post-message-callback (&rest args)
  (declare (ignore args))
  (loop for message = (lparallel.queue:try-pop-queue *post-message-queue*)
        while message
        do (funcall message))
    iup:+default+)

(defun iup-loop ()
  (iup:with-iup ()
    (setf *post-message-handler* (iup:user :postmessage_cb 'post-message-callback))
    (setf (iup:global :lockloop) :yes)
    (iup:main-loop)))

;; #-sbcl (iup-loop)<

;; #+sbcl
;; (sb-int:with-float-traps-masked
;;     (:divide-by-zero :invalid)
;;   ())

(defun start ()
  (bt:make-thread #'iup-loop :name "iup-main-loop"))

(defun stop ()
  (call-with-main-loop-asynchronous #'iup:exit-loop)
  (setf *post-message-queue* nil))

(defun call-with-main-loop-asynchronous (func)
  (lparallel.queue:push-queue func *post-message-queue*)
  (iup-cffi::%iup-post-message
   *post-message-handler* (cffi:null-pointer) 42 pi (cffi:null-pointer)))

#+nil
(progn
  (loop repeat 100 for i from 0 do
    (call-with-main-loop-asynchronous
     (lambda ()
       (format t "called from UI thread: ~a~%" i)))))

#+nil
(call-with-main-loop-asynchronous
 (lambda ()
   (let* ((text (iup:text :expand :yes :multiline :yes))
          (vbox (iup:vbox (list text)))
          (dialog (iup:dialog vbox :size "QUARTERxQUARTER")))
     (iup:show dialog))))
