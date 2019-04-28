(defpackage #:iup-examples.pomodoro
  (:use #:common-lisp)
  (:export #:pomodoro))

(in-package #:iup-examples.pomodoro)

(defun timer-complete (handle)
  (declare (ignore handle))
  (iup:message "Pomodoro" "Interval complete!")
  iup:+default+)

(defun start (handle)
  (declare (ignore handle)))

(defun tasks (handle)
  (declare (ignore handle))
  (let* ((name (iup:text :expand "HORIZONTAL"))
	 (description (iup:text :multiline "YES" :expand "YES"))
	 (vbox (iup:vbox (list name description)))
	 (dialog (iup:dialog vbox)))
    (unwind-protect
	 (iup:popup dialog iup:+centerparent+ iup:+centerparent+)
      (iup:destroy dialog)))
  iup:+default+)

(defvar *interval-time* 25)
(defvar *total-time* nil)

(defun pomodoro ()
  (iup:with-iup ()
    (let* ((time-label (iup:label :title "--:--" :size "x16" :fontsize 32 :alignment "ACENTER:ACENTER" :expand "YES"))
	   (time-progress (iup:progress-bar  :expand "HORIZONTAL"))
	   (task-label (iup:label :title "No task description"))
	   (start-button (iup:button :title "Start" :fgcolor "green" :expand "HORIZONTAL"))
	   (cancel-button (iup:button :title "Cancel" :active "NO" :expand "HORIZONTAL"))
	   (tasks-button (iup:button :title "Tasks..." :action 'tasks :expand "HORIZONTAL"))
	   (hbox (iup:hbox (list start-button cancel-button tasks-button)))
	   (vbox (iup:vbox (list time-label time-progress task-label hbox)))
	   (dialog (iup:dialog vbox :title "Pomodoro"))
	   (timer (iup:timer :run "NO" :action_cb 'timer-complete)))
      (setf (iup:callback start-button :action)
	    #'(lambda (self)
		(setf (iup:attribute cancel-button :active) "YES"
		      (iup:attribute self :active) "NO"
		      (iup:attribute timer :run) "YES"
		      *total-time*  (* 60 *interval-time*)
		      (iup:attribute time-progress :max) *total-time*
		      (iup:attribute time-progress :value) (/ *total-time* 2))
		
		iup:+default+)
	    (iup:callback cancel-button :action)
	    #'(lambda (self)
		(setf (iup:attribute start-button :active) "YES"
		      (iup:attribute self :active) "NO"
		      (iup:attribute timer :run) "NO"
		      (iup:attribute timer :time) 10000)
		iup:+default+))
      (iup:show dialog)
      (iup:main-loop))))

#-sbcl (pomodoro)
#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (pomodoro))
