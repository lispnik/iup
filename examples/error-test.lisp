(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "iup"))

(defpackage #:iup-examples.error-test
  (:use #:common-lisp)
  (:export #:error-test))

(in-package #:iup-examples.error-test)

(defun error-test ()
  (iup:with-iup ()
    (let* ((label (iup:label :title "Nobody likes restarting their lisp! Our IUP bindings
should recover from all errors. Check the button 
tooltip for a description."
			     :padding "x5"))
	   (button1 (iup:button :title "Success"
				:action 'success-callback
				:tip "A successful callback example"))
	   (button2 (iup:button :title "Error in Callback"
				:action 'error-callback
				:tip "Error occurs inside the callback. This will trigger a restart case where you can specify USE-DEFAULT which will simulate a callback returning IUP:+DEFAULT+. Look in the REPL for the restarts."))
	   (button3 (iup:button :title "Ooopsie Callback"
				:action 'oopsie-callback
				:tip "The callback was specified as a symbol, but there's nothing funcallable. This will trigger a restart case where you can specify USE-DEFAULT which will simulate a callback returning IUP:+DEFAULT+. Look in the REPL for the restarts."))
	   (button4 (iup:button :title "Forgot Return"
				:action 'forgot-callback
				:tip "Callbacks should return IUP:+DEFAULT+, IUP:+IGNORE+ or IUP:+CLOSE+, but during development we'll forget to do that (a lot). When we do, this will trigger a restart case where you can specify USE-DEFAULT which will simulate a callback returning IUP:+DEFAULT+. Look in the REPL for the restarts."))
	   (vbox (iup:vbox (list label button1 button2 button3 button4)
			   :expand :yes))
           (dialog (iup:dialog vbox :title "Error Test")))
      (iup:show dialog)
      (iup:main-loop))))

(defun success-callback (handle)
  (iup:message "Error Test" "Successful callback")
  iup:+default+)

(defun error-callback (handle)
  (error "Error in callback")
  iup:+default+)

#-sbcl (error-test)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (error-test))
