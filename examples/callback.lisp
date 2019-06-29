;;; Generated from org-mode, do not edit

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload "iup"))

(defpackage #:iup-examples.callback
  (:use #:common-lisp)
  (:export #:callback))

(in-package #:iup-examples.callback)

(defun callback ()
  (iup:with-iup ()
    (let* ((button1
             (iup:button :title "Test &1"
                         :expand :yes
                         :tip "Callback inline at control creation"
                         :action (lambda (handle)
                                   (message "button1's action callback")
                                   iup:+default+)))
           (button2
             (iup:button :title "Test &2"
                         :expand :yes
                         :tip "Callback set later using (SETF (IUP:CALLBACK ..) ..)"))
           (button3
             (iup:button :title "Test &3"
                         :expand :yes
                         :tip "Callback example using symbol-referenced function at control creation"
                         :action 'test3-callback))
           (button4
             (iup:button :title "Test &4"
                         :expand :yes
                         :tip "Callback example using symbol-referenced function later using (SETF (IUP:CALLBACK ..) ..)"))
           (vbox
             (iup:vbox (list button1 button2 button3 button4)
                       :gap "10"
                       :margin "10x10"
                       :alignment :acenter))
           (dialog
             (iup:dialog vbox :title "Callback Example")))
      (setf (iup:callback button2 :action)
            (lambda (handle)
              (message "button2's action callback")
              iup:+default+))
      (setf (iup:callback button4 :action) 'test4-callback)
      (iup:show dialog)
      (iup:main-loop))))

(defun test3-callback (handle)
  (message "button3's action callback")
  iup:+default+)

(defun test4-callback (handle)
  (message "button4's action callback")
  iup:+default+)

(defun message (message)
  (iup:message "Callback Example" message))

#-sbcl (callback)

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (callback))
