(in-package #:iup)

(export '(idle-action))

(defvar *idle-action* nil)

(cffi:defcallback %idle-action :int ()
  (if *idle-action*
      (let ((result (restart-case
                        (funcall *idle-action*)
                      (:use-default ()
                       :report "Continue by returning IUP:+DEFAULT+"
                        iup:+default+))))
        (cond
          ((= result iup:+ignore+)
           (setf (idle-action) nil))
          ((or (= result iup:+default+)
               (= result iup:+close+))
           result)
          (t (restart-case
                 (error "Callback returned invalie value ~A" result)
               (:use-default ()
                :report "Continue by returning IUP:+DEFAULT+"
                 iup:+default+)))))
      iup:+ignore+))

(defun (setf idle-action) (new-action)
  "NEW-ACTION should take no arguments and return IUP:+DEFAULT+,
IUP:+IGNORE+ or IUP:+CLOSE+."
  (setf *idle-action* new-action)
  (if new-action
      (iup-cffi::%iup-set-function :idle_action (cffi:callback %idle-action))
      (iup-cffi::%iup-set-function :idle_action (cffi:null-pointer)))
  new-action)

(defun idle-action () *idle-action*)
