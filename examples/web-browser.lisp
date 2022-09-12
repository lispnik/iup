(ql:quickload '("iup-web"))

(defpackage #:iup-examples.web-browser
  (:use #:common-lisp)
  (:export #:web-browser))

(in-package #:iup-examples.web-browser)

(defvar *web* nil)

(defvar *url* "http://www.lispworks.com/documentation/HyperSpec/Front/index.htm")

(defun back (handle)
  (declare (ignore handle))
  (setf (iup:attribute *web* :backforward) -1)
  iup:+default+)

(defun forward (handle)
  (declare (ignore handle))
  (setf (iup:attribute *web* :backforward) 1)
  iup:+default+)

(defun stop (handle)
  (declare (ignore handle))
  (setf (iup:attribute *web* :stop) nil)
  iup:+default+)

(defun reload (handle)
  (declare (ignore handle))
  (setf (iup:attribute *web* :reload) nil)
  iup:+default+)

(defun goto (handle)
  (declare (ignore handle))
  (setf (iup:attribute *web* :value) *url*)
  iup:+default+)

(defun web-browser ()
  (iup:with-iup ()
    (iup-web:open)
    (let* ((btn-back (iup:button :title "Back" :action 'back))
	   (btn-forward (iup:button :title "Forward" :action 'forward))
	   (text (iup:text :expand :horizontal
                           :value *url*
                           :valuechanged_cb #'(lambda (handle)
                                                (setf *url* (iup:attribute handle :value))
                                                iup:+default+)))
	   (btn-load (iup:button :title "Load" :action 'goto))
	   (btn-reload (iup:button :title "Reload" :action 'reload))
	   (btn-stop (iup:button :title "Stop" :action 'stop))
	   (web (iup-web:web-browser :expand "YES"))
	   (dialog (iup:dialog 
		    (iup:vbox
		     (list (iup:hbox
			    (list btn-back
				  btn-forward
				  text
				  btn-load
				  btn-reload
				  btn-stop)
			    :margin "5x5"
			    :gap "5")
			   web))
		    :title "IupWebBrowser"
		    :rastersize "800x600"
		    :map_cb #'(lambda (handle)
				(declare (ignore handle))
				(setf *web* web)
				(goto nil)
				iup:+default+))))
      (iup:show dialog)
      (iup:main-loop))))

#+sbcl
(sb-int:with-float-traps-masked
    (:divide-by-zero :invalid)
  (web-browser))

#-sbcl (web-browser)
