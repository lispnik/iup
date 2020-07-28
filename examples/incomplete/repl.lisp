(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '("iup" "prepl" "trivial-gray-streams")))

(defpackage #:iup-examples.repl
  (:use #:common-lisp
	#:alexandria))

(in-package #:iup-examples.repl)

(defclass repl-output-stream
    (trivial-gray-streams:fundamental-character-output-stream)
  ((handle :initarg :handle
           :initform (error "Text control is required"))))

(defmethod trivial-gray-streams:stream-line-column ((stream repl-output-stream))
  (with-slots (handle)
      stream
    (let ((lin-col (iup:attribute handle :caret)))
      (parse-number:parse-number
       lin-col
       :start (1+ (position #\, lin-col :test #'char=))))))

(defmethod trivial-gray-streams:stream-write-char ((stream repl-output-stream) character)
  (with-slots (handle)
      stream
    (iup::call-with-main-loop
     (lambda ()
       (setf (iup:attribute handle :append)
             (string character))))))

(iup::start)

(sleep 1)

(defparameter text (iup:text :multiline :yes :expand :yes :appendnewline :no))
(defparameter dialog (iup:dialog text :size "400x300"))

(sleep 1)

(iup::call-with-main-loop
 (lambda ()
   (iup:show dialog)))

(sleep 2)

(let* ((*standard-output* (make-instance 'repl-output-stream :handle text)))
  (prepl:repl))
