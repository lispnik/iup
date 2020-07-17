(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "bordeaux-threads" "lparallel")))

(defpackage #:iup-examples.threads
  (:use #:common-lisp)
  (:export #:threads))

(in-package #:iup-examples.threads)

(defvar *post-message-handler*)

(defvar *post-message-queue* (lparallel.queue:make-queue))

(defun post-message-callback (&rest args)
  (format t "postmessage_cb args: ~a~%" args)
  (terpri)
  (loop for message = (lparallel.queue:try-pop-queue *post-message-queue* :timeout 5)
        while message
        do (funcall message))
    iup:+default+)

(defun iup-loop ()
  (iup:with-iup ()
    (setf *post-message-handler* (iup:user :postmessage_cb 'post-message-callback))
    (setf (iup:global :lockloop) :yes)
    (iup:main-loop)))

;; #-sbcl (iup-loop)

;; #+sbcl
;; (sb-int:with-float-traps-masked
;;     (:divide-by-zero :invalid)
;;   ())


#+nil (iup:exit-loop)
#+nil (bt:make-thread #'iup-loop :name "iup-main-loop")

#+nil
(defun call-with-iup-main-loop (func)
  (lparallel.queue:push-queue func *post-message-queue*)
  (iup-cffi::%iup-post-message
   *post-message-handler*
   (cffi:null-pointer)
   42
   pi
   (cffi:null-pointer)))

(progn
  (loop repeat 100 for i from 0 do
    (call-with-iup-main-loop
     (lambda ()
       (format t "called from UI thread: ~a~%" i)))))
